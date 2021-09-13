
/*
<graph> ::= <header> <graphContent>   // Graph(dir: Dir, content: GraphContent)
<header> ::= graph (TD|LR)<newline>   // Direction can be TD or LR
<graphContent> ::= <atomicGraph> | <compoundGraph> -----------type
<atomicGraph> ::= <nodeDecl>
<compoundGraph> ::= <edge>+
<edge> ::= <node> --> <edgeLabel>? <node><newline>   // <edgeLabel> is optional
// Edge(from: Node, to: Node, label?: string)
<node> ::= <nodeDecl> | <nodeRef>-----------type
<nodeDecl> ::= <identifier>["<string>"]   // NodeDecl(id: string, label: string)
<nodeRef> ::= <identifier>   // NodeRef(id: string)
<edgeLabel> ::= |<identifier>|   // string
*/

// A value returned by parse
export type GraphContent = AtomicGraph | CompoundGraph;
export type Node = NodeDecl | NodeRef;
export type Direction = "TD" | "LR";

export interface Graph {tag:"Graph"; dir: Direction; content: GraphContent;}
export interface AtomicGraph {tag:"AtomicGraph"; val: NodeDecl;}
export interface CompoundGraph {tag:"CompoundGraph"; edges: Edge[];}
export interface Edge {tag:"Edge"; from: Node; to: Node; lable?: string;}
export interface NodeDecl {tag:"NodeDecl"; id: string; lable: string; }
export interface NodeRef {tag:"NodeRef"; id: string;}
//export interface Header {tag: "Header"; dir: Direction;}
/****************************/
//export interface TD {tag: "TD"; }
//export interface LR {tag: "LR"; }

// Type value constructors for disjoint types
export const makeGraph = (dir: Direction, content: GraphContent): Graph => ({tag: "Graph", dir: dir, content: content});
export const makeAtomicGraph = (val: NodeDecl): AtomicGraph => ({tag: "AtomicGraph", val: val}); 
export const makeCompoundGraph = (edges: Edge[]): CompoundGraph => ({tag: "CompoundGraph", edges:edges}); 
export const makeEdge = (from: Node, to: Node, lable?: string): Edge => ({tag: "Edge", from: from, to: to, lable: lable}); 
export const makeNodeDecl = (id: string, lable: string): NodeDecl => ({tag:"NodeDecl", id: id, lable: lable}); 
export const makeNodeRef = (id: string): NodeRef => ({tag: "NodeRef", id: id});
//
//export const makeTD = (): TD => ({tag:"TD"});
//export const makeLR = (): LR => ({tag:"LR"});


// Type predicates for disjoint types

export const isGraph = (x: any): x is Graph => x.tag === "Graph";
export const isAtomicGraph = (x: any): x is AtomicGraph => x.tag === "AtomicGraph";
export const isCompoundGraph = (x: any): x is CompoundGraph => x.tag === "CompoundGraph";
export const isEdge = (x: any): x is Edge => x.tag === "Edge";
export const isNodeDecl = (x: any): x is NodeDecl => x.tag === "NodeDecl";
export const isNodeRef = (x: any): x is NodeRef => x.tag === "NodeRef";
//
//export const isTD = (x: any): x is TD => x.tag === "TD";
//export const isLR = (x: any): x is LR => x.tag === "LR";

export const isGraphContent = (x: any): x is GraphContent => 
    isAtomicGraph(x) || isCompoundGraph(x);
export const isNode = (x: any): x is Node => 
    isNodeDecl(x) || isNodeRef(x);
export const isDirection = (x: any): x is Direction =>
    x === "TD" || x === "LR";


