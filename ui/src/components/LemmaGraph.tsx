import dagre from "@dagrejs/dagre";
import { Background, Controls, Handle, Position, ReactFlow, useNodesState, type Edge, type Node, type NodeProps } from "@xyflow/react";
import "@xyflow/react/dist/style.css";
import { useEffect, useMemo } from "react";
import { shownStatus, type GraphState, type Mark, type NodeState, type SplitState } from "../board";
import css from "./board.module.css";

const goalW = 250;
const goalH = 78;
const splitW = 170;
const splitH = 34;

type GoalData = { goal: NodeState };
type SplitData = { split: SplitState };

export function MarkChips({ marks }: { marks: Mark[] }) {
  if (!marks.length) return null;
  return (
    <span className={css.marks}>
      {marks.map((m, i) => (
        <span key={i} className={`${css.mark} ${css[m.kind.replace(/ /g, "_")] ?? ""}`} data-testid="mark" data-kind={m.kind} title={m.title ?? m.text}>
          {m.label ?? m.kind}
          {m.label === undefined && m.text ? `: ${m.text}` : ""}
        </span>
      ))}
    </span>
  );
}

function GoalNode({ data }: NodeProps<Node<GoalData>>) {
  const g = data.goal;
  const st = shownStatus(g);
  return (
    <div className={`${css.goalNode} ${css[`st_${st.replace(/ /g, "_")}`] ?? ""}`} data-testid={`node-${g.id}`} data-status={st} data-depth={g.depth}>
      <Handle type="target" position={Position.Top} />
      <div className={css.nodeName} title={g.name}>
        {g.name}
      </div>
      <div className={css.nodeMeta}>
        <span className={css.status}>{st}</span>
        {g.reason && <span title={g.reason}>{g.reason}</span>}
      </div>
      <MarkChips marks={g.marks} />
      <Handle type="source" position={Position.Bottom} />
    </div>
  );
}

function SplitNode({ data }: NodeProps<Node<SplitData>>) {
  const sp = data.split;
  return (
    <div className={`${css.splitNode} ${css[`sp_${sp.status}`] ?? ""}`} data-testid={`split-${sp.id}`} data-status={sp.status} title={sp.reason ?? undefined}>
      <Handle type="target" position={Position.Top} />
      split {sp.id}: {sp.status}
      <Handle type="source" position={Position.Bottom} />
    </div>
  );
}

const nodeTypes = { goal: GoalNode, split: SplitNode };

/** The graph laid out top to bottom by dagre: goal, its splits, their lemmas. */
export function layout(g: GraphState): { nodes: Node[]; edges: Edge[] } {
  const d = new dagre.graphlib.Graph();
  d.setGraph({ rankdir: "TB", nodesep: 30, ranksep: 46 });
  d.setDefaultEdgeLabel(() => ({}));
  const goalHeight = (x: NodeState) => goalH + (x.marks.length ? 20 : 0);
  for (const x of g.nodes.values()) d.setNode(`n${x.id}`, { width: goalW, height: goalHeight(x) });
  for (const sp of g.splits.values()) d.setNode(`s${sp.id}`, { width: splitW, height: splitH });
  const edges: Edge[] = [];
  for (const sp of g.splits.values()) {
    if (g.nodes.has(sp.parent)) {
      d.setEdge(`n${sp.parent}`, `s${sp.id}`);
      edges.push({ id: `e-n${sp.parent}-s${sp.id}`, source: `n${sp.parent}`, target: `s${sp.id}` });
    }
    for (const l of sp.lemmas) {
      if (!g.nodes.has(l)) continue;
      d.setEdge(`s${sp.id}`, `n${l}`);
      edges.push({ id: `e-s${sp.id}-n${l}`, source: `s${sp.id}`, target: `n${l}` });
    }
  }
  dagre.layout(d);
  const nodes: Node[] = [];
  for (const x of g.nodes.values()) {
    const p = d.node(`n${x.id}`);
    nodes.push({ id: `n${x.id}`, type: "goal", position: { x: p.x - goalW / 2, y: p.y - goalHeight(x) / 2 }, data: { goal: x }, draggable: false });
  }
  for (const sp of g.splits.values()) {
    const p = d.node(`s${sp.id}`);
    nodes.push({ id: `s${sp.id}`, type: "split", position: { x: p.x - splitW / 2, y: p.y - splitH / 2 }, data: { split: sp }, draggable: false });
  }
  return { nodes, edges };
}

export default function LemmaGraph({ graph }: { graph: GraphState }) {
  const laid = useMemo(() => layout(graph), [graph]);
  // the flow keeps each node's measured size in its own state; a new
  // layout replaces the nodes
  const [nodes, setNodes, onNodesChange] = useNodesState<Node>(laid.nodes);
  useEffect(() => setNodes(laid.nodes), [laid, setNodes]);
  const edges = laid.edges;
  if (!graph.nodes.size) return <p className="muted">No goals yet.</p>;
  return (
    <div className={css.graph} data-testid="lemma-graph">
      <ReactFlow
        key={`${graph.nodes.size}-${graph.splits.size}`}
        nodes={nodes}
        edges={edges}
        nodeTypes={nodeTypes}
        onNodesChange={onNodesChange}
        fitView
        nodesConnectable={false}
        nodesDraggable={false}
        elementsSelectable={false}
        minZoom={0.2}
      >
        <Background />
        <Controls showInteractive={false} />
      </ReactFlow>
    </div>
  );
}
