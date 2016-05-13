open System
open System.IO
open System.Collections.Generic
open System.Text
open YamlDotNet.Core
open YamlDotNet.RepresentationModel
open System.Xml

type YamlObject = 
    | Asset
    | NwsPost
    | EvtEvent
    | Outlander

type OutputDocs = Dictionary<string,XmlDocument>

type YamlProcessor = (YamlObject -> string -> OutputDocs -> unit)
    
let itemType = function
    | "Asset" -> Asset
    | "NwsPost" -> NwsPost
    | "EvtEvent" -> EvtEvent 
    | _ -> Outlander  // a match antyhing match

let printYamlObject = function
    | Asset -> "Asset"
    | NwsPost -> "NwsPost"
    | EvtEvent -> "EvtEvent"
    | Outlander -> "Outlander"

let yamlObjectType (line:string) = 
    let item = line.Split( [|':'|], 2).[1].Trim()
    item |> itemType

let classObject (line:string) = 
    match (line.StartsWith "--- !ruby/object") with 
        | false -> None
        | true -> Some (line |> yamlObjectType)
        
let (++) x y = x + y + "\n"

let documents (fn:YamlProcessor) source outDocs  =
    let translate (lastYaml, document) line = 
            match (classObject line) with 
                | Some nextYamlObject -> 
                    match lastYaml with 
                        | Some y -> fn y document outDocs
                        | _ -> ()
                    (Some nextYamlObject, "" ++ line)
                | None ->  (lastYaml, document ++ line)

    let lastYaml, document = source  |> Seq.fold translate (None, "")

    match lastYaml with
        | Some y -> fn y document outDocs
        | _ -> ()



let mappingKey (entry:KeyValuePair<YamlNode,YamlNode>) = (entry.Key :?> YamlScalarNode).Value

let rec toXml (xmlRoot:XmlNode) (xmlDocument:XmlDocument) (root:YamlNode) =
    match root with
        | :? YamlMappingNode as mapping ->
                    for entry in mapping.Children do
                        let e = xmlDocument.CreateElement(mappingKey entry)
                        let r = xmlRoot.AppendChild(e)
                        toXml r xmlDocument entry.Value
        | :? YamlScalarNode as scalar -> xmlRoot.InnerText <- scalar.Value
        | :? YamlSequenceNode as seq ->
                    let recurse = toXml xmlRoot xmlDocument
                    Seq.iter recurse seq.Children
        | _ -> printf "Unrecognized node: %s" (root.ToString())
    printfn ""

let createWriter (filename:string) : XmlWriter =
    let settings = new XmlWriterSettings(Indent = true)
    let outFile = sprintf "../../%s.xml" filename
    XmlWriter.Create(outFile, settings)

let parseDocument (yamlObject:YamlObject) (document:string) (outDocs:OutputDocs) =
    match yamlObject with
        | Asset | EvtEvent | NwsPost -> 
            let xmlDocument = outDocs.[printYamlObject yamlObject]
            let newYaml = xmlDocument.CreateElement(printYamlObject yamlObject)
            let root = xmlDocument.FirstChild.AppendChild(newYaml)
            let yaml = new YamlStream()
            yaml.Load(new StringReader(document))
            toXml root xmlDocument (yaml.Documents.[0].RootNode)
        | _ -> ()

let createDoc (outDocs:OutputDocs) name = 
    let d = new XmlDocument()
    d.AppendChild(d.CreateElement(name + "s")) |> ignore
    outDocs.Add(name, d)
    name

let writeOut (outDocs:OutputDocs) name =                
    printfn "Wrote %s.xml" name
    let out = outDocs.[name]
    use w = createWriter name
    out.WriteTo(w)

[<EntryPoint>]
let main args = 

    let createDocs names =     
        let source = File.ReadAllLines(@"../Debug/backup.yml") 
            
        let outDocs = new OutputDocs()

        let create = Seq.map (createDoc outDocs)
        let write = Seq.iter (writeOut outDocs)
        let parse names = documents parseDocument source outDocs; names

        create >> parse >> write |> ignore

    ["Asset"; "NwsPost"; "EvtEvent";] |> createDocs
    0




