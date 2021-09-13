//functions 'mapL4toGraph', 'unparseGraph', 'L4toGraph' be written into this file
import { Result, makeFailure, bind, makeOk, mapResult, safe2, safe3 } from "../shared/result";
import { Parsed, Exp, Program, DefineExp, AtomicExp, VarDecl, CExp, CompoundExp,
         isExp, isProgram, isDefineExp, isCExp, isAtomicExp, isCompoundExp, isNumExp, isBoolExp,
         isStrExp, isPrimOp, isVarRef, isAppExp, isIfExp, isProcExp, isLetExp,
         isLitExp, isLetrecExp, isSetExp, AppExp, IfExp, ProcExp, LetExp, Binding, LitExp, LetrecExp, SetExp, parseL4, parseL4Exp } from "./L4-ast";
import { Graph, Edge, Node, GraphContent,
         makeNodeDecl, makeEdge, makeCompoundGraph, makeGraph, makeNodeRef,  makeAtomicGraph
         ,isAtomicGraph, isCompoundGraph, NodeRef, isNodeDecl, isNodeRef } from "./mermaid-ast";
import { SExpValue, isClosure, isSymbolSExp, isEmptySExp, isCompoundSExp, Closure, SymbolSExp, CompoundSExp, Value } from "./L4-value";
import { isNumber, isBoolean, isString } from "../shared/type-predicates";
import { Env,  ExtEnv } from "./L4-env";
import { rest } from "../shared/list";

import { parse } from '../shared/parser';
import { Sexp } from 's-expression';


// ========================================================
//****Question: 2.2****
// Parsing

//Purpose: create a generator of new strings of the form: v_n
const makeVarGen = (): ((v: string) => string) => {
    let count: number = 0;
    return (v: string) => {
        count++;
        return `${v}_${count}`;
    }
}

//converts a L4 parsed value to a mermaid grapgh
export const mapL4toMermaid = (p: Parsed): Result<Graph> => {
    const final_result: Result<GraphContent> =
        isExp(p) ? expToGraph(p) : //if we got a single exp..
        isProgram(p) ? programToGraph(p) : // if we got an array of exps - program..
        makeFailure(`Unexpected type ` + p); //if we got something else..
    return bind(final_result, (graphContent: GraphContent) => makeOk(makeGraph(`TD`, graphContent)));
};

//convert a L4 exp to it's mermaid virsion 
const expToGraph = (exp: Exp): Result<GraphContent> =>
    isDefineExp(exp) ? defineExpToGraph(exp) :
    isCExp(exp) ? cexpToGraph(exp) :
    makeFailure(`Unexpected type ` + exp);


const cexpToGraph = (cexp: CExp): Result<GraphContent> =>
    isAtomicExp(cexp) ? atomicExpToGraph(cexp) :
    isCompoundExp(cexp) ? compoundExpToGraph(cexp) :
    makeFailure(`Unexpected type ` + cexp);


//if we did't get an exp, then we got a program
//this function is dealing with a program: convert it to a graph content
const programGenerator:((v:string)=>string) = makeVarGen();
const expsGenerator:((v:string)=>string) = makeVarGen();

const programToGraph = (program: Program): Result<GraphContent> => {
    const exps_id: string = expsGenerator(`Exps`);
    return bind(serialize_nodes_2(makeOk([makeEdge(makeNodeDecl(programGenerator(`Program`), "Program"), makeNodeDecl(exps_id, ":"), "exps")]),
                                    bind(mapResult((x: Exp) => expToGraph(x),program.exps),
                                    (contents: GraphContent[]) => serialize_nodes(makeNodeRef(exps_id), contents))),(edges: Edge[]) => 
                                    makeOk(makeCompoundGraph(edges)));
};

const varDeclGenerator:((v:string)=>string) = makeVarGen();

const varDeclToGraph = (varDecl: VarDecl): Result<GraphContent> =>
    makeOk(makeAtomicGraph(makeNodeDecl(varDeclGenerator(`VarDecl`), `"${varDecl.tag}(${varDecl.var})"`)));


const defineExpGenerator:((v:string)=>string) = makeVarGen();

const defineExpToGraph = (defineExp: DefineExp): Result <GraphContent> => {
    const define_id: string = defineExpGenerator(`DefineExp`);  

    const right_edges: Result<Edge[]> = bind(cexpToGraph(defineExp.val), (content: GraphContent) => 
                                    serialize_one_node(makeNodeRef(define_id), content, "val"));
    const left_edge: Result<Edge[]> = bind(varDeclToGraph(defineExp.var), (content: GraphContent) => 
                                    serialize_one_node(makeNodeDecl(define_id, "DefineExp"), content, "var"));
    return bind(serialize_nodes_2(left_edge, right_edges), (x: Edge[]) => makeOk(makeCompoundGraph(x)) );
};

//1
const numExpGenerator:((v:string)=>string)  = makeVarGen();
const boolExpGenerator:((v:string)=>string) = makeVarGen();
const strExpGenerator:((v:string)=>string)  = makeVarGen();
const primOpGenerator:((v:string)=>string)  = makeVarGen();
const varRefGenerator:((v:string)=>string)  = makeVarGen();

const atomicExpToGraph = (atomicExp: AtomicExp): Result<GraphContent> =>
    isNumExp(atomicExp) ? makeOk(makeAtomicGraph(makeNodeDecl(numExpGenerator(`NumExp`), `"${atomicExp.tag}(${atomicExp.val})"`))) :
    isBoolExp(atomicExp) ? makeOk(makeAtomicGraph(makeNodeDecl(boolExpGenerator(`BoolExp`), `"${atomicExp.tag}(${atomicExp.val})"`))) :
    isStrExp(atomicExp) ? makeOk(makeAtomicGraph(makeNodeDecl(strExpGenerator(`StrExp`), `"${atomicExp.tag}(${atomicExp.val})"`))) :
    isPrimOp(atomicExp) ? makeOk(makeAtomicGraph(makeNodeDecl(primOpGenerator(`PrimOp`), `"${atomicExp.tag}(${atomicExp.op})"`))) :
    isVarRef(atomicExp) ? makeOk(makeAtomicGraph(makeNodeDecl(varRefGenerator(`VarRef`), `"${atomicExp.tag}(${atomicExp.var})"`))) :
    makeFailure(`Unexpected type ` + atomicExp);

/////type CompoundExp = AppExp | IfExp | ProcExp | LetExp | LitExp | LetrecExp | SetExp;
const compoundExpToGraph = (compoundExp: CompoundExp): Result<GraphContent> =>
    isAppExp(compoundExp) ? appExpToGraph(compoundExp) :
    isIfExp(compoundExp) ? ifExpToGraph(compoundExp) :
    isProcExp(compoundExp) ? procExpToGraph(compoundExp) :
    isLetExp(compoundExp) ? letExpToGraph(compoundExp) :
    isLitExp(compoundExp) ? litExpToGraph(compoundExp) :
    isLetrecExp(compoundExp) ? letrecExpToGraph(compoundExp) :
    isSetExp(compoundExp) ? setExpToGraph(compoundExp) :
    makeFailure(`Unexpected type ` + compoundExp);

//1
const appExpGenerator:((v:string)=>string) = makeVarGen();
const randsGenerator:((v:string)=>string) = makeVarGen();

const appExpToGraph = (appExp: AppExp): Result<GraphContent> => {
    
    const app_id: string = appExpGenerator(`AppExp`);
    const rands_id: string = randsGenerator(`Rands`);

    const left_edge: Result<Edge[]> = bind(cexpToGraph(appExp.rator), (content: GraphContent) => 
                                        serialize_one_node(makeNodeDecl(app_id, "AppExp"), content, "rator"));
    const right_edge: Result<Edge[]> = makeOk([makeEdge(makeNodeRef(app_id), makeNodeDecl(rands_id, ":"), "rands")]);
    const right_arr_edge: Result<Edge[]> = bind(mapResult((cexp: CExp) => cexpToGraph(cexp) , appExp.rands), (contents: GraphContent[]) => 
                                        serialize_nodes(makeNodeRef(rands_id), contents));
    return bind(serialize_nodes_2(left_edge, right_edge, right_arr_edge), (edges: Edge[]) => makeOk(makeCompoundGraph(edges)));
};

//2
const ifExpGenerator:((v:string)=>string) = makeVarGen();

const ifExpToGraph = (ifExp: IfExp): Result<GraphContent> => {
    
    const if_id: string = ifExpGenerator(`IfExp`);

    const left_edge: Result<Edge[]> = bind(cexpToGraph(ifExp.test), (content: GraphContent) => 
                                        serialize_one_node(makeNodeDecl(if_id, "IfExp"), content, "test"));
    const center_node: Result<Edge[]> = bind(cexpToGraph(ifExp.then), (content: GraphContent) => 
                                        serialize_one_node(makeNodeRef(if_id), content, "then"));
    const right_edge: Result<Edge[]> = bind(cexpToGraph(ifExp.alt), (content: GraphContent) => 
                                        serialize_one_node(makeNodeRef(if_id), content, "alt"));
    return bind(serialize_nodes_2(left_edge, center_node, right_edge), (edges: Edge[]) => makeOk(makeCompoundGraph(edges)));
};

//3
const procExpGenerator:((v:string)=>string) = makeVarGen();
const argsGenerator:((v:string)=>string) = makeVarGen();
const bodyGenerator:((v:string)=>string) = makeVarGen();

const procExpToGraph = (procExp: ProcExp): Result<GraphContent> => {
    const proc_id: string = procExpGenerator(`ProcExp`);
    const args_id: string = argsGenerator(`Params`);
    const body_id: string = bodyGenerator(`Body`);

    const left_edge:      Result<Edge[]> = makeOk([makeEdge(makeNodeDecl(proc_id, "ProcExp"), makeNodeDecl(args_id, ":"), "args")]);
    const left_arr_edge:  Result<Edge[]> = bind(mapResult( (varDecl: VarDecl) => varDeclToGraph(varDecl) , procExp.args), (contents: GraphContent[]) => 
                                        serialize_nodes(makeNodeRef(args_id), contents ) );
    const right_edge:     Result<Edge[]> = makeOk([makeEdge(makeNodeRef(proc_id), makeNodeDecl(body_id, ":"), "body")]);
    const right_arr_edge: Result<Edge[]> = bind(mapResult( (cexp: CExp) => cexpToGraph(cexp) , procExp.body), (contents: GraphContent[]) => 
                                        serialize_nodes(makeNodeRef(body_id), contents ) );
    return bind(serialize_nodes_2(left_edge, left_arr_edge, right_edge, right_arr_edge), (edges: Edge[]) => makeOk(makeCompoundGraph(edges)));
};

//4
const letExpGenerator:((v:string)=>string) = makeVarGen();
const bindingsGenerator:((v:string)=>string) = makeVarGen();

const letExpToGraph = (letExp: LetExp): Result<GraphContent> => {
    const let_id:      string = letExpGenerator(`LetExp`);
    const body_id:     string = bodyGenerator(`Body`);
    const bindings_id: string = bindingsGenerator(`Bindings`);

    const left_edge: Result<Edge[]> = makeOk([makeEdge(makeNodeDecl(let_id, "LetExp"), makeNodeDecl(bindings_id, ":"), "bindings")]);
    const left_arr_edge: Result<Edge[]> = bind(mapResult( (binding: Binding) => 
                                    bindingToGraph(binding) , letExp.bindings), (contents: GraphContent[]) => 
                                    serialize_nodes(makeNodeRef(bindings_id), contents ) );
    const right_edge: Result<Edge[]> = makeOk([makeEdge(makeNodeRef(let_id), makeNodeDecl(body_id, ":"), "body")]);
    const right_arr_edge: Result<Edge[]> = bind(mapResult( (cexp: CExp) => cexpToGraph(cexp) , letExp.body), (contents: GraphContent[]) => 
                                    serialize_nodes(makeNodeRef(body_id), contents ) );
    return bind(serialize_nodes_2(left_edge, left_arr_edge, right_edge, right_arr_edge), (edges: Edge[]) => makeOk(makeCompoundGraph(edges)));
};

//5
const litExpGenerator:((v:string)=>string) = makeVarGen();

const litExpToGraph = (litExp: LitExp): Result<GraphContent> => {
    const lit_id: string = litExpGenerator(`LitExp`);

    const center_node: Result<Edge[]> = bind(sexpValueToGraph(litExp.val), (content: GraphContent) => 
                                    serialize_one_node(makeNodeDecl(lit_id, "LitExp"), content, "val"));
    return bind(center_node, (edges: Edge[]) => makeOk(makeCompoundGraph(edges)));
};

//6
const letrecExpGenerator:((v:string)=>string) = makeVarGen();

const letrecExpToGraph = (letrecExp: LetrecExp): Result<GraphContent> => {
    const letrec_id:   string = letrecExpGenerator(`LetrecExp`);
    const bindings_id: string = bindingsGenerator(`Bindings`);
    const body_id:     string = bodyGenerator(`Body`); 

    const left_edge: Result<Edge[]> = makeOk([makeEdge(makeNodeDecl(letrec_id, "LetExp"), makeNodeDecl(bindings_id, ":"), "bindings")]);
    const bindingsToContents: Result<GraphContent[]> = mapResult( (binding: Binding) => 
                                        bindingToGraph(binding) , letrecExp.bindings);
    const left_arr_edge: Result<Edge[]> = bind(bindingsToContents, (contents: GraphContent[]) => 
                                        serialize_nodes(makeNodeRef(bindings_id), contents));
    const right_edge: Result<Edge[]> = makeOk([makeEdge(makeNodeRef(letrec_id), makeNodeDecl(body_id, ":"), "body")]);
    const cexpsToGrapgCont: Result<GraphContent[]> = mapResult( (cexp: CExp) => 
                                        cexpToGraph(cexp) , letrecExp.body);
    const right_arr_edge: Result<Edge[]> = bind(cexpsToGrapgCont, (contents: GraphContent[]) => 
                                        serialize_nodes(makeNodeRef(body_id), contents ) );
    return bind(serialize_nodes_2(left_edge, left_arr_edge, right_edge, right_arr_edge), (edges: Edge[]) => 
                                        makeOk(makeCompoundGraph(edges)));
};

//7
const setExpGenerator:((v:string)=>string) = makeVarGen();

const setExpToGraph = (setExp: SetExp): Result<GraphContent> => {
    const set_id: string = setExpGenerator(`SetExp`);

    const left_edge: Result<Edge[]> = bind(cexpToGraph(setExp.var), (content: GraphContent) =>
                                    serialize_one_node(makeNodeDecl(set_id, "SetExp"), content, "var"));
    const right_edge: Result<Edge[]> = bind(cexpToGraph(setExp.val), (content: GraphContent) => 
                                    serialize_one_node(makeNodeRef(set_id), content, "val"));
    return bind(serialize_nodes_2(left_edge, right_edge), (edges: Edge[]) => makeOk(makeCompoundGraph(edges)));
};

/////the end of the compoundExpToGraph part

const bindingGenerator:((v:string)=>string) = makeVarGen();

const bindingToGraph = (binding: Binding): Result<GraphContent> => {
    const binding_id: string = bindingGenerator(`Binding`);

    const left_edge: Result<Edge[]> = bind(varDeclToGraph(binding.var), (content: GraphContent) => 
                                    serialize_one_node(makeNodeDecl(binding_id, "Binding"), content, "var"));
    const right_edge: Result<Edge[]> = bind(cexpToGraph(binding.val), (content: GraphContent) => 
                                    serialize_one_node(makeNodeRef(binding_id), content, "val"));
    return bind(serialize_nodes_2(left_edge, right_edge), (edges: Edge[]) => makeOk(makeCompoundGraph(edges)));
};

//<sexp>    ::= symbol | number | bool | string | ( <sexp>* )
const numberGenerator:((v:string)=>string) = makeVarGen();
const booleanGenerator:((v:string)=>string) = makeVarGen();
const stringGenerator:((v:string)=>string) = makeVarGen();
const emptySExpGenerator:((v:string)=>string) = makeVarGen();

const sexpValueToGraph = (sexpValue: SExpValue): Result<GraphContent> =>
    isSymbolSExp(sexpValue) ? symbolSExpToGraph(sexpValue) :
    isNumber(sexpValue) ? makeOk(makeAtomicGraph(makeNodeDecl(numberGenerator(`Number`), `"number(${sexpValue})"`))) :
    isBoolean(sexpValue) ? makeOk(makeAtomicGraph(makeNodeDecl(booleanGenerator(`Boolean`), `"boolean(${sexpValue})"`))) :
    isString(sexpValue) ? makeOk(makeAtomicGraph(makeNodeDecl(stringGenerator(`String`), `"string(${sexpValue})"`))) :
    
    isPrimOp(sexpValue) ? atomicExpToGraph(sexpValue) ://1
    isClosure(sexpValue) ? closureToGraph(sexpValue) ://2
    isEmptySExp(sexpValue) ? makeOk(makeAtomicGraph(makeNodeDecl(emptySExpGenerator(`EmptySExp`), `"EmptySExp"`))) ://3
    isCompoundSExp(sexpValue) ? compoundSExpToGraph(sexpValue) ://4
    makeFailure(`Unexpected type `+ sexpValue);

//2
const paramsGenerator:((v:string)=>string) = makeVarGen();
const closureGenerator:((v:string)=>string) = makeVarGen();

const closureToGraph = (closure: Closure): Result<GraphContent> => {
    const params_id:  string = paramsGenerator(`Params`);
    const closure_id: string = closureGenerator(`Closure`);
    const body_id:    string = bodyGenerator(`Body`);

    const left_edge: Result<Edge[]> = makeOk([makeEdge(makeNodeDecl(closure_id, "Closure"), makeNodeDecl(params_id, ":"), "params")]);
    const left_arr_edge: Result<Edge[]> = bind(mapResult( (varDecl: VarDecl) => varDeclToGraph(varDecl) , closure.params), (contents: GraphContent[]) => 
                                    serialize_nodes(makeNodeRef(params_id), contents ) );
    const center_node: Result<Edge[]> = makeOk([makeEdge(makeNodeRef(closure_id), makeNodeDecl(body_id, ":"), "body")]);
    const arr_of_nodes: Result<Edge[]> = bind(mapResult( (cexp: CExp) => cexpToGraph(cexp) , closure.body), (contents: GraphContent[]) => 
                                    serialize_nodes(makeNodeRef(body_id), contents ) );
    const right_edge: Result<Edge[]> = bind(envToGraph(closure.env), (content: GraphContent) => 
                                    serialize_one_node(makeNodeRef(closure_id), content, "env"));
    return bind(serialize_nodes_2(left_edge, left_arr_edge, center_node, arr_of_nodes, right_edge), (edges: Edge[]) => makeOk(makeCompoundGraph(edges)));
};


const extEnvGenerator:((v:string)=>string) = makeVarGen();
const varsGenerator:((v:string)=>string) = makeVarGen();
const valsGenerator:((v:string)=>string) = makeVarGen();

const extEnvToGraph = (extEnv: ExtEnv): Result<GraphContent> => {
    const ext_id:  string = extEnvGenerator(`ExtEnv`);
    const vars_id: string = varsGenerator(`Vars`);
    const vals_id: string = valsGenerator(`Vals`);

    const left_edge: Result<Edge[]> = makeOk([makeEdge(makeNodeDecl(ext_id, "ExtEnv"), makeNodeDecl(vars_id, ":"), "vars")]);
    const stringsToContents: Result<GraphContent[]> = mapResult( (string: string) => 
                                    sexpValueToGraph(string) , extEnv.vars);
    const left_arr_edge: Result<Edge[]> = bind(stringsToContents, (contents: GraphContent[]) => 
                                    serialize_nodes(makeNodeRef(vars_id), contents ) );
    const center_node: Result<Edge[]> = makeOk([makeEdge(makeNodeRef(ext_id), makeNodeDecl(vals_id, ":"), "vals")]);
    const arr_of_nodes: Result<Edge[]> = bind( mapResult( (value: Value) => sexpValueToGraph(value) , extEnv.vals), (contents: GraphContent[]) => 
                                    serialize_nodes(makeNodeRef(vals_id), contents ) );
    const right_edge: Result<Edge[]> = bind(envToGraph(extEnv.nextEnv), (content: GraphContent) => 
                                    serialize_one_node(makeNodeRef(ext_id), content, "nextEnv"));
    return bind(serialize_nodes_2(left_edge, left_arr_edge, center_node, arr_of_nodes, right_edge), (edges: Edge[]) => makeOk(makeCompoundGraph(edges)));
};


const emptyEnvGenerator:((v:string)=>string) = makeVarGen();

const envToGraph = (env: Env): Result<GraphContent> => 
    env.tag === "EmptyEnv" ? makeOk(makeAtomicGraph(makeNodeDecl(emptyEnvGenerator(`EmptyEnv`), `"EmptyEnv"`))) :
    env.tag === "ExtEnv" ? extEnvToGraph(env) : 
    makeFailure(`Unexpected type `+ env);
///////

const symbolSExpGenerator:((v:string)=>string) = makeVarGen();

const symbolSExpToGraph = (symbolSExp: SymbolSExp): Result<GraphContent> =>
    bind(serialize_nodes_2(bind(sexpValueToGraph(symbolSExp.val), (content: GraphContent) =>  
                                            serialize_one_node(makeNodeRef(symbolSExpGenerator(`SymbolSExp`)), content, "val"))), (edges: Edge[]) => 
                                            makeOk(makeCompoundGraph(edges)));

const compoundSExpGenerator:((v:string)=>string) = makeVarGen();

const compoundSExpToGraph = (compoundSExp: CompoundSExp): Result<GraphContent> => {
    const comp_id: string = compoundSExpGenerator(`CompoundSExp`);

    const left_edge: Result<Edge[]> = bind(sexpValueToGraph(compoundSExp.val1), (content: GraphContent) => 
                                        serialize_one_node(makeNodeDecl(comp_id, "CompoundSExp"), content, "val1"));
    const right_edge: Result<Edge[]> = bind(sexpValueToGraph(compoundSExp.val2), (content: GraphContent) => 
                                        serialize_one_node(makeNodeRef(comp_id), content, "val2"));
    return bind(serialize_nodes_2(left_edge, right_edge), (edges: Edge[]) => makeOk(makeCompoundGraph(edges)));
};


//The utility functions:
//get one node and a graph content as parameters, then build an edge between them
const serialize_one_node = (node: Node, content: GraphContent, lable?: string): Result<Edge[]> =>
    isAtomicGraph(content) ? makeOk([makeEdge(node, content.val, lable)]) :
    isCompoundGraph(content) ?
        content.edges.length === 0 ? makeFailure(`Cant build a compound graph with no edges`) :
        isNodeRef(content.edges[0]) ? makeOk([makeEdge(node, content.edges[0].from, lable)].concat(content.edges[0])) :
        makeOk([makeEdge(node, content.edges[0].from, lable)].concat(  
             [makeEdge(makeNodeRef(content.edges[0].from.id), content.edges[0].to, content.edges[0].lable)].concat(rest(content.edges)))) :
    makeFailure(`Cant build a compound graph with the given content`);


const serialize_nodes = (node: NodeRef, exps: GraphContent[], lable?: string): Result<Edge[]> => 
     bind(mapResult( (content: GraphContent) => serialize_one_node(node, content, lable) , exps), (edges: Edge[][]) => 
                                        makeOk(edges.reduce( (accumulate, current) => accumulate.concat(current), <Edge[]>[])));

const serialize_nodes_2 = (...args: Result<Edge[]>[]): Result<Edge[]> =>
    args.length === 0 ? makeFailure(`The function -serialize_nodes_2- did't not get arguments`) :
    args.length === 1 ? args[0] : 
    args.length === 2 ? safe2( (first_edge: Edge[], second_edge: Edge[]) => 
                                        makeOk(first_edge.concat(second_edge)))(args[0], args[1]) :
    args.length === 3 ? safe3( (first_edge: Edge[], second_edge: Edge[], third_edge: Edge[]) => 
                                        makeOk(first_edge.concat(second_edge).concat(third_edge)))(args[0], args[1], args[2]) :
    args.length === 4 ? safe4( (first_edge: Edge[], second_edge: Edge[], third_edge: Edge[], fourth_edge: Edge[]) => 
                                        makeOk(first_edge.concat(second_edge).concat(third_edge).concat(fourth_edge)))(args[0], args[1], args[2], args[3]) :
    args.length === 5 ? safe5( (first_edge: Edge[], second_edge: Edge[], third_edge: Edge[], fourth_edge: Edge[], fifth_edge: Edge[]) => 
                                        makeOk(first_edge.concat(second_edge).concat(third_edge).concat(fourth_edge).concat(fifth_edge)))(args[0], args[1], args[2], args[3], args[4]) :
    makeFailure(`The function -serialize_nodes_2- can't get more than 5 arguments`);

//the safe functions, which we use in order to handle errors while mapping
const safe4 = <T1, T2, T3, T4, T5>(f: (x: T1, y: T2, z: T3, v: T4) => Result<T5>): (xr: Result<T1>, yr: Result<T2>, zr: Result<T3>, vr: Result<T4>) => Result<T5> =>
    (xr: Result<T1>, yr: Result<T2>, zr: Result<T3>, vr: Result<T4>) =>
        bind(xr, (x: T1) => bind(yr, (y: T2) => bind(zr, (z: T3) => bind(vr, (v: T4) => f(x, y, z, v)))));

const safe5 = <T1, T2, T3, T4, T5, T6>(f: (x: T1, y: T2, z: T3, v: T4, u: T5) => Result<T6>): (xr: Result<T1>, yr: Result<T2>, zr: Result<T3>, vr: Result<T4>, ur: Result<T5>) => Result<T6> =>
        (xr: Result<T1>, yr: Result<T2>, zr: Result<T3>, vr: Result<T4>, ur: Result<T5>) =>
            bind(xr, (x: T1) => bind(yr, (y: T2) => bind(zr, (z: T3) => bind(vr, (v: T4) => bind(ur, (u: T5) => f(x, y, z, v, u))))));


//****Question: 2.3**** 
//the unparse part
const unparseGraphContent = (content: GraphContent): string =>
    isAtomicGraph(content) ? `${content.val.id}[${content.val.lable}]` :
    isCompoundGraph(content) ? content.edges.reduce((accumulate, current) => accumulate + unparseGraphEdge(current), ``) :``;


const unparseGraphEdge = (edge: Edge): string =>
    edge.lable === undefined ?
    `${unparseGraphNode(edge.from)} --> ${unparseGraphNode(edge.to)}\n` :
    `${unparseGraphNode(edge.from)} -->|${edge.lable}| ${unparseGraphNode(edge.to)}\n`;


const unparseGraphNode = (node: Node): string =>
    isNodeDecl(node) ? `${node.id}[${node.lable}]` :
    isNodeRef(node) ? `${node.id}` : ``;

//the main functions of this part:
//get a L4 program (or other exp) as a string and unparse it to get a mermaid grapgh according to the memaid "language"
export const L4toMermaid = (concrete: string): Result<string> =>
    !concrete.includes("L4") ? 
            bind(parse(concrete), (s_exp: Sexp) => 
            bind(parseL4Exp(s_exp), (exp: Exp) => 
            bind(mapL4toMermaid(exp), (grapgh1: Graph) => unparseMermaid(grapgh1)))):
    /*else */    bind(parseL4(concrete), (exp: Program) => bind(mapL4toMermaid(exp), (grapgh2: Graph) => unparseMermaid(grapgh2))) ;

//
export const unparseMermaid = (exp: Graph): Result<string> => 
                        makeOk(`graph ${exp.dir}\n${unparseGraphContent(exp.content)}`);


