import { Map, Set } from "immutable";
import React, { useEffect, useState } from "react";
import Splits, { ActiveSplits, SplitGroup } from "./Cube/Splits";
import { Link, useParams } from "react-router-dom";
import Api, { Cube, Visualisation, WithHash, CubeHash, DimensionGroup, Measure, Dimension } from "../Api";
import { HiAcademicCap, HiAnnotation, HiCalendar, HiChevronDown, HiChevronRight, HiChevronUp, HiClock, HiOutlineStop, HiShieldCheck, HiStop, HiX } from "react-icons/hi";
import Page from "./Page";
import { Observable, Subject, combineLatestWith, map, mergeMap, scan, startWith } from "rxjs";
import { ImRadioChecked, ImRadioUnchecked } from "react-icons/im";
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
import Measures, { ActiveMeasures } from "./Cube/Measures";

export type Change<F> = (f: F) => F

const CubeTitle: React.FC<{ cubeId: string }> = ({ cubeId }) => {
  const [cubeTitle, setCubeTitle] = useState<string>(cubeId);

  useEffect(() => {
    const sub = Api.cube(cubeId).subscribe(cube => setCubeTitle(cube.name));
    return () => sub.unsubscribe();
  }, [cubeId])

  return <span>{cubeTitle}</span>
}

const DimensionGroupIcon: React.FC<{ dimensionGroup: DimensionGroup }> = ({ dimensionGroup }) => {
  switch (dimensionGroup.primary_dimension.type) {
    case "Nominal":
      return <HiAnnotation />
    case "Quantitative":
      return <HiShieldCheck />
    case "Ordinal":
      return <HiAcademicCap />
    case "TemporalAbs":
      return <HiCalendar />
    case "TemporalRel":
      return <HiClock />
    default:
      return <HiAnnotation />
  }
}

const FieldGroupItemRep: React.FC<{ fgKey: string, dimensionGroups: Map<string, DimensionGroup> }> = ({ fgKey, dimensionGroups }) => {
  const group = dimensionGroups.get(fgKey);
  return <div className="items-center flex flex-row p-2 bg-slate-200 mx-2 hover:bg-slate-300 text-slate-600 hover:text-slate-800 hover:cursor-pointer">
    <div className="w-8 ">{group ? <DimensionGroupIcon dimensionGroup={group} /> : "N/A"}</div>
    <div>{group ? group.name : "N/A"}</div>
  </div>
}

const DimensionGroupItem: React.FC<{ dimensionGroup: WithHash<DimensionGroup> }> = ({ dimensionGroup }) => {
  const { attributes, listeners, setNodeRef, transform, isDragging } = useDraggable({
    id: dimensionGroup[0],
    data: dimensionGroup
  });
  const style = {
    transform: CSS.Translate.toString(transform),
  };

  return isDragging ? <></> : <div style={style} className="items-center flex flex-row p-2 bg-slate-200 mx-2 hover:bg-slate-300 text-slate-600 hover:text-slate-800 hover:cursor-pointer" {...listeners} {...attributes} ref={setNodeRef}>
    <div className="w-8 "><DimensionGroupIcon dimensionGroup={dimensionGroup[1]} /></div>
    <div>{dimensionGroup[1].name}</div>
  </div>
}

const DimensionList: React.FC<{ dimensions: WithHash<DimensionGroup>[] }> = ({ dimensions }) => {
  return <div className="p-2 m-2 rounded bg-slate-200 flex flex-col gap-4">
    <span className="uppercase">Dimensions</span>
    <ul key="dimension list" className="flex flex-col gap-y-1 py-1">{dimensions.map((f) => <DimensionGroupItem key={f[0]} dimensionGroup={f} />)}</ul>
  </div>
}

const MeasureItem: React.FC<{ measure: WithHash<Measure>, activeMeasures: Observable<ActiveMeasures>, measureChange: Subject<Change<ActiveMeasures>> }> = ({measure, activeMeasures, measureChange}) => {
  const [activeMeasure, setActiveMeasure] = useState<boolean>(false);

  function toggle(): void {
    setActiveMeasure(!activeMeasure)
    measureChange.next(x => Measures.toggleMeasure(x, measure[1].sql_field_name));
  }

  return <div onClick={() => toggle()} className="items-center flex flex-row p-2 bg-slate-200 mx-2 hover:bg-slate-300 text-slate-600 hover:text-slate-800 hover:cursor-pointer">
    <div className="h-4 w-4 mr-2">
      { activeMeasure ? <ImRadioChecked/> : <ImRadioUnchecked/>  }
    </div>
    <div>{measure[1].display_name || measure[1].sql_field_name}</div>
  </div>
}

const MeasureList: React.FC<{measures: WithHash<Measure>[], activeMeasures: Observable<ActiveMeasures>, measureChange: Subject<Change<ActiveMeasures>> }> = ({measures, activeMeasures, measureChange}) => {
  return <div className="p-2 m-2 rounded bg-slate-200 flex flex-col gap-4">
    <span className="uppercase">Measures</span>
    <ul key="measures list" className="flex flex-col gap-y-1 py-1">{measures.map((f) => <MeasureItem measure={f} key={f[0]} measureChange={measureChange} activeMeasures={activeMeasures} />)}</ul>
  </div>

}

const SplitLabel: React.FC<{ selChange: Subject<Change<ActiveSplits>>, currentGroup: SplitGroup, splitKey: number }> = ({ currentGroup, selChange, splitKey }) => {
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
        {currentGroup.selectedField.display_name || currentGroup.selectedField.sql_field_name}
      </span>
      <HiX onClick={() => selChange.next(x => Splits.deleteDimensionGroup(x, splitKey))} className={dStyle} />
    </div>
      {isOpen && (
        <FloatingFocusManager context={context} modal={true}>
          <div ref={refs.setFloating} style={floatingStyles} {...getFloatingProps()} className="bg-blue-200 text-slate-800 py-2 flex flex-col gap-y-2 w-64 z-40">
            {Api.allOutFields(currentGroup.fieldGroup).map((f, index) => <SplitLabelField dimension={f} splitKey={splitKey} selChange={selChange} key={index} />)}
          </div>
        </FloatingFocusManager>
      )}
  </div>
}

const SplitLabelField: React.FC<{dimension: Dimension, splitKey: number, selChange: Subject<Change<ActiveSplits>>}> = ({dimension, splitKey, selChange}) => {
  function setField() {
    selChange.next(x => Splits.setSelectedField(x, splitKey, dimension));
  }
  return <div className="hover:bg-blue-100 mx-2 p-2 cursor-pointer" onClick={() => setField()}>
    <span>{dimension.display_name || dimension.sql_field_name}</span>
  </div>
}

const SplitSelection: React.FC<{ currentSplits: Observable<ActiveSplits>, selChange: Subject<Change<ActiveSplits>> }> = ({ currentSplits, selChange }) => {
  const [activeFieldGroups, setActiveFieldGroups] = useState<ActiveSplits>(Map());
  useEffect(() => {
    const subs = currentSplits.subscribe(setActiveFieldGroups);
    return () => subs.unsubscribe();
  }, [])
  const { setNodeRef, isOver, active } = useDroppable({ id: "split-selection" });
  const bgColor = active ? (isOver ? "bg-slate-300 border-slate-800 border-4 border-dashed" : "bg-slate-300 border-slate-800 border-2 border-dashed") : "bg-slate-200";
  const className = "rounded w-64 h-12 flex items-center flex-row px-2 " + bgColor;

  return <div className="bg-slate-200 m-2 flex flex-row items-center h-16 rounded">
    <div key="lbl"><span className="uppercase mx-2">Splits</span></div>
    {activeFieldGroups.toArray().map((entry) => <SplitLabel currentGroup={entry[1]} selChange={selChange} splitKey={entry[0]} key={entry[0]} />)}
    <div key="drop-target" ref={setNodeRef} className={className}>{active ? "Add split" : ""}</div>
  </div>
}

const VisualisationComp: React.FC<{ vis: WithHash<Visualisation> }> = ({ vis }) => {
  const [data, setData] = useState<any[]>([]);
  useEffect(() => {
    const sub = Api.evl({ q: vis[1].visQuery, query: vis[1].visApiQuery}).subscribe(setData);
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
const VisList: React.FC<{ currentSplits: Observable<ActiveSplits>, currentMeasures: Observable<ActiveMeasures>, cube: Cube, cubeHash: CubeHash }> = ({ currentSplits, cube, cubeHash, currentMeasures }) => {
  const [visualisations, setVisualisations] = useState<WithHash<Visualisation>[]>([]);
  const [selectedVis, setSelectedVis] = useState<WithHash<Visualisation> | undefined>();
  useEffect(() => {
    const sub = currentSplits.pipe(
      combineLatestWith(currentMeasures),
      map(x => Api.selections(Splits.splits(x[0]).concat(x[1].toArray()) )),
      mergeMap(selections => Api.vis({ q: cubeHash, selections }))
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
  const [dimensionGroups, setDimensionGroups] = useState<Map<string, DimensionGroup>>(Map());

  const [activeId, setActiveId] = useState<UniqueIdentifier>();

  const [splitChange] = useState<Subject<Change<ActiveSplits>>>(new Subject());
  const [measureChange] = useState<Subject<Change<ActiveMeasures>>>(new Subject());

  // Active field groups with index
  // These contribute to the selections 
  const [selections] = useState<Observable<ActiveSplits>>(
    splitChange.pipe(
      scan((acc, f) => f(acc), Map<number, SplitGroup>()),
      startWith(Map<number, SplitGroup>())
    )
  );

  useEffect(() => {
    const sub4 = Api.cube(cubeId).subscribe(x => {
      setCube(x);
      setDimensionGroups(Map(x.dimensions));
    });
    return () => { sub4.unsubscribe(); }
  }, [cubeId])

  const [activeMeasures] = useState<Observable<ActiveMeasures>>(
    measureChange.pipe(
      scan((acc, f) => f(acc), Set<string>())
    )
  )

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
      const k = dimensionGroups.get(f.active.id.toString());
      k && splitChange.next(x => Splits.addDimensionGroup(x, k));
    }
  }

  return <Page navs={[]} title={<CubeTitle cubeId={cubeId} />} top={breadcrumbs}>
    <div className="flex flex-row flex-0 h-full">
      {cube && (
        <DndContext onDragStart={f => setActiveId(f.active.id)} onDragEnd={endDrag} onDragCancel={f => setActiveId(undefined)}>
          <div className="flex flex-col w-72 bg-slate-100 overflow-y-auto">
            <DimensionList dimensions={cube.dimensions} />
            <MeasureList measures={cube.measures} activeMeasures={activeMeasures} measureChange={measureChange}/>
          </div>
          <div style={{ height: "100%" }} className="flex flex-grow flex-col bg-slate-100" key="vis-cards">
            <SplitSelection currentSplits={selections} selChange={splitChange} />
            <div className="grid my-2 mx-2 gap-2 flex-grow bottom-0 overflow-y-auto bg-slate-100" key="vis-cards">
              <VisList cube={cube} currentSplits={selections} cubeHash={cubeId} currentMeasures={activeMeasures} />
            </div>
          </div>
          <DragOverlay>{activeId ? <FieldGroupItemRep fgKey={activeId.toString()} dimensionGroups={dimensionGroups} /> : null}</DragOverlay>
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