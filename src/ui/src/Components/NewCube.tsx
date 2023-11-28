import { Map } from "immutable";
import React, { useEffect, useState } from "react";
import Types, { ActiveFields, ActiveFieldGroup } from "./Types";
import { Link, useParams } from "react-router-dom";
import Api, { Cube, FieldGroup, OutField, Visualisation, WithHash } from "../Api";
import { HiAcademicCap, HiAnnotation, HiCalendar, HiChevronDown, HiChevronRight, HiChevronUp, HiClock, HiShieldCheck, HiX } from "react-icons/hi";
import Page from "./Page";
import { Observable, Subject, map, mergeMap, scan, startWith } from "rxjs";
import { DndContext, DragEndEvent, DragOverlay, UniqueIdentifier, useDraggable, useDroppable } from '@dnd-kit/core';
import { CSS } from '@dnd-kit/utilities';
import { VegaLite } from "react-vega";
import {
  useFloating,
  autoUpdate,
  offset,
  flip,
  shift,
  useClick,
  useDismiss,
  useRole,
  useInteractions,
  FloatingFocusManager,
} from '@floating-ui/react';

export type Change<F> = (f: F) => F

const CubeTitle: React.FC<{ cubeId: string }> = ({ cubeId }) => {
  const [cubeTitle, setCubeTitle] = useState<string>(cubeId);

  useEffect(() => {
    const sub = Api.cube(cubeId).subscribe(cube => setCubeTitle(cube.name));
    return () => sub.unsubscribe();
  }, [cubeId])

  return <span>{cubeTitle}</span>
}

const FieldGroupIcon: React.FC<{ fieldGroup: FieldGroup }> = ({ fieldGroup }) => {
  switch (fieldGroup.primary_field.field_options.tag) {
    case "nominal":
      return <HiAnnotation />
    case "quantitative":
      return <HiShieldCheck />
    case "ordinal":
      return <HiAcademicCap />
    case "temporal-abs":
      return <HiCalendar />
    case "temporal-rel":
      return <HiClock />
    default:
      return <HiAnnotation />
  }
}

const FieldGroupItemRep: React.FC<{ fgKey: string, fieldGroups: Map<string, FieldGroup> }> = ({ fgKey, fieldGroups }) => {
  const group = fieldGroups.get(fgKey);
  return <div className="items-center flex flex-row p-2 bg-slate-200 mx-2 hover:bg-slate-300 text-slate-600 hover:text-slate-800 hover:cursor-pointer">
    <div className="w-8 ">{group ? <FieldGroupIcon fieldGroup={group} /> : "N/A"}</div>
    <div>{group ? group.name : "N/A"}</div>
  </div>
}

const FieldGroupItem: React.FC<{ fieldGroup: WithHash<FieldGroup> }> = ({ fieldGroup }) => {
  const { attributes, listeners, setNodeRef, transform, isDragging } = useDraggable({
    id: fieldGroup[0],
    data: fieldGroup
  });
  const style = {
    transform: CSS.Translate.toString(transform),
  };

  return isDragging ? <></> : <div style={style} className="items-center flex flex-row p-2 bg-slate-200 mx-2 hover:bg-slate-300 text-slate-600 hover:text-slate-800 hover:cursor-pointer" {...listeners} {...attributes} ref={setNodeRef}>
    <div className="w-8 "><FieldGroupIcon fieldGroup={fieldGroup[1]} /></div>
    <div>{fieldGroup[1].name}</div>
  </div>
}

const FieldsList: React.FC<{ fields: WithHash<FieldGroup>[] }> = ({ fields }) => {
  return <div className="p-2 m-2 rounded bg-slate-200 flex flex-col gap-4">
    <span className="uppercase">Fields</span>
    <ul key="fields list" className="flex flex-col gap-y-1 py-1">{fields.map((f) => <FieldGroupItem key={f[0]} fieldGroup={f} />)}</ul>
  </div>
}

const SplitLabel: React.FC<{ selChange: Subject<Change<ActiveFields>>, currentGroup: ActiveFieldGroup, splitKey: number }> = ({ currentGroup, selChange, splitKey }) => {
  const [isOpen, setIsOpen] = useState<boolean>(false);
  const { refs, floatingStyles, context } = useFloating({
    open: isOpen,
    onOpenChange: setIsOpen,
    middleware: [offset({mainAxis: 17, crossAxis: 0}), flip(), shift()],
    whileElementsMounted: autoUpdate,
  });

  const click = useClick(context);
  const dismiss = useDismiss(context);
  const role = useRole(context);

  // Merge all the interactions into prop getters
  const { getReferenceProps, getFloatingProps } = useInteractions([
    click,
    dismiss,
    role,
  ]);

  const dStyle = "w-6 h-6 hover:bg-blue-800"
  return <div>
    <div className="rounded hover:cursor-pointer h-12 bg-blue-200 w-64 m-2 px-2 items-center flex flex-row hover:text-slate-100 hover:bg-blue-400">
      <div className="w-4 h-4 text-blue-200 hover:text-slate-100 mr-2">
        { isOpen ? <HiChevronUp/> : <HiChevronDown/> }
      </div>
      <span className="flex-grow" ref={refs.setReference} {...getReferenceProps()}>
        {currentGroup.outField.display_name || currentGroup.outField.sql_field_name}
      </span>
      <HiX onClick={() => selChange.next(x => Types.deleteFieldGroup(x, splitKey))} className={dStyle} />
    </div>
      {isOpen && (
        <FloatingFocusManager context={context} modal={true}>
          <div ref={refs.setFloating} style={floatingStyles} {...getFloatingProps()} className="bg-blue-200 text-slate-800 py-2 flex flex-col gap-y-2 w-64 z-40">
            {Api.allOutFields(currentGroup.fieldGroup).map((f, index) => <SplitLabelField outField={f} splitKey={splitKey} selChange={selChange} key={index} />)}
          </div>
        </FloatingFocusManager>
      )}
  </div>
}

const SplitLabelField: React.FC<{outField: OutField, splitKey: number, selChange: Subject<Change<ActiveFields>>}> = ({outField, splitKey, selChange}) => {
  function setField() {
    selChange.next(x => Types.setOutField(x, splitKey, outField));
  }
  return <div className="hover:bg-blue-100 mx-2 p-2 cursor-pointer" onClick={() => setField()}>
    <span>{outField.display_name || outField.sql_field_name}</span>
  </div>
}

const SplitSelection: React.FC<{ currentSelection: Observable<ActiveFields>, selChange: Subject<Change<ActiveFields>> }> = ({ currentSelection, selChange }) => {
  const [activeFieldGroups, setActiveFieldGroups] = useState<ActiveFields>(Map());
  useEffect(() => {
    const subs = currentSelection.subscribe(setActiveFieldGroups);
    return () => subs.unsubscribe();
  }, [])
  const { setNodeRef, isOver, active } = useDroppable({ id: "split-selection" });
  const bgColor = active ? (isOver ? "bg-slate-300 border-slate-800 border-4 border-dashed" : "bg-slate-300 border-slate-800 border-2 border-dashed") : "bg-slate-200";
  const className = "rounded w-64 h-12 flex items-center flex-row px-2 " + bgColor;

  return <div className="bg-slate-200 m-2 flex flex-row items-center h-16 rounded">
    <div key="lbl"><span className="uppercase mx-2">Selections</span></div>
    {activeFieldGroups.toArray().map((entry) => <SplitLabel currentGroup={entry[1]} selChange={selChange} splitKey={entry[0]} key={entry[0]} />)}
    <div key="drop-target" ref={setNodeRef} className={className}>{active ? "Add split" : ""}</div>
  </div>
}

const VisualisationComp: React.FC<{ vis: WithHash<Visualisation> }> = ({ vis }) => {
  const [data, setData] = useState<any[]>([]);
  useEffect(() => {
    const sub = Api.evl({ q: vis[1].visQuery, fields: vis[1].visFields }).subscribe(setData);
    return () => sub.unsubscribe();
  }, [vis[1].visQuery])
  return <VegaLite className="h-full w-full border-left border-top border-right border-slate-300" spec={vis[1].visDefinition} actions={false} data={{ table: data }} />
}

const NoVisualisationsComp: React.FC<{}> = () => {
  return <div className="h-full w-full  border-left border-top border-right border-slate-300">No visualisations</div>
}

const VisEntry: React.FC<{ vis: WithHash<Visualisation>, setSelected: (v: WithHash<Visualisation> | undefined) => void, selectedVis: (WithHash<Visualisation> | undefined) }> = ({ vis, setSelected, selectedVis }) => {
  const bg = (selectedVis && (selectedVis[0] == vis[0])) ? " bg-white cursor:pointer hover:bg-slate-100" : " bg-slate-300 hover:bg-white"
  const className = "h-20 w-20 mx-2 mb-2 p-2 flex flex-col items-center text-xl " + bg;
  return <div key={vis[0]} onClick={() => setSelected(vis)} className={className}>
    {vis[1].visArchetype.substring(0, 10)}
  </div>
}

/**
 * 
 * @param param0 Observable with the current selections, and the cube that we are looking at
 * @returns A component that shows the graph
 */
const VisList: React.FC<{ currentSelection: Observable<ActiveFields>, cube: Cube }> = ({ currentSelection, cube }) => {
  const [visualisations, setVisualisations] = useState<WithHash<Visualisation>[]>([]);
  const [selectedVis, setSelectedVis] = useState<WithHash<Visualisation> | undefined>();
  useEffect(() => {
    const sub = currentSelection.pipe(
      map(x => Types.selections(x)),
      mergeMap(selections => Api.vis({ q: cube.query, selections }))
    ).subscribe(x => {
      setVisualisations(x);
      (x.length > 0) ? setSelectedVis(x[0]) : setSelectedVis(undefined);
    });
    return () => sub.unsubscribe();
  }, [cube]);

  return <div className="flex flex-col flex-grow">
    <div className="bg-slate-50 flex flex-col flex-grow overflow-y-none">
      {selectedVis ? <VisualisationComp vis={selectedVis} key={selectedVis[0]} /> : <NoVisualisationsComp />}
    </div>
    <div className="flex flex-row gap-x-2 h-24 bg-slate-200">
      {visualisations.map(x => <VisEntry vis={x} setSelected={setSelectedVis} selectedVis={selectedVis} />)}
    </div>
  </div>
}

const CubePage: React.FC<{ cubeId: string }> = ({ cubeId }) => {
  const [cube, setCube] = useState<Cube | undefined>();

  // Field groups with their hash
  // These can be dragged from the sidebar
  const [fieldGroups, setFieldGroups] = useState<Map<string, FieldGroup>>(Map());

  const [activeId, setActiveId] = useState<UniqueIdentifier>();

  const [selChange] = useState<Subject<Change<ActiveFields>>>(new Subject());

  // Active field groups with index
  // These contribute to the selections 
  const [selections] = useState<Observable<ActiveFields>>(
    selChange.pipe(
      scan((acc, f) => f(acc), Map<number, ActiveFieldGroup>()),
      startWith(Map<number, ActiveFieldGroup>())
    )
  );

  useEffect(() => {
    const sub4 = Api.cube(cubeId).subscribe(x => {
      setCube(x);
      setFieldGroups(Map(x.fields));
    });
    return () => { sub4.unsubscribe(); }
  }, [cubeId])

  const breadcrumbs = <div className="flex flex-row gap-1 text-slate-400 align-middle items-center justify-start h-8">
    <Link to="/"><span>Home</span></Link>
    <HiChevronRight />
    <Link to="/cubes"><span>Cubes</span></Link>
    <HiChevronRight />
    <Link to={`/cubes/${cubeId}`}><span className="text-slate-300">{cube ? cube.display_name : cubeId}</span></Link>
  </div>

  function endDrag(f: DragEndEvent): void {
    setActiveId(undefined);
    if (f.over) {
      const k = fieldGroups.get(f.active.id.toString());
      k && selChange.next(x => Types.addFieldGroup(x, k));
    }
  }

  return <Page navs={[]} title={<CubeTitle cubeId={cubeId} />} top={breadcrumbs}>
    <div className="flex flex-row flex-0 h-full">
      {cube && (
        <DndContext onDragStart={f => setActiveId(f.active.id)} onDragEnd={endDrag} onDragCancel={f => setActiveId(undefined)}>
          <div className="flex flex-col w-72 bg-slate-100 overflow-y-auto">
            <FieldsList fields={cube.fields} />
          </div>
          <div style={{ height: "100%" }} className="flex flex-grow flex-col bg-slate-100" key="vis-cards">
            <SplitSelection currentSelection={selections} selChange={selChange} />
            <div className="grid my-2 mx-2 gap-2 flex-grow bottom-0 overflow-y-auto bg-slate-100" key="vis-cards">
              <VisList cube={cube} currentSelection={selections} />
            </div>
          </div>
          <DragOverlay>{activeId ? <FieldGroupItemRep fgKey={activeId.toString()} fieldGroups={fieldGroups} /> : null}</DragOverlay>
        </DndContext>
      )}
    </div>
  </Page>
}

const CubeComponent: React.FC = () => {
  const params = useParams();
  const cubeId = params["cubeid"] || "";
  if (cubeId === "") { console.warn("CubePage: Cube ID not found in URL params") };
  return <CubePage cubeId={cubeId} />
}

export default CubeComponent;