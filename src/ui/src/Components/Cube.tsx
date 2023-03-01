import React, { ReactNode, useEffect, useState } from "react";
import Api, { Archetype, Cube, Field, Measurement, Selections, Visualisation } from "../Api";
import { Link, useParams } from "react-router-dom";
import Page from "./Page";
import { combineLatestWith, map, mergeMap, Observable, scan, startWith, Subject } from "rxjs";
import { VegaLite } from "react-vega";
import { HiViewGrid, HiChevronRight } from "react-icons/hi";
import { AiOutlineDotChart, AiOutlineHeatMap, AiOutlineLineChart } from "react-icons/ai";
import { TfiBarChart, TfiCalendar, TfiDashboard, TfiDownload, TfiText, TfiTime } from "react-icons/tfi";
import { Lens } from 'monocle-ts';

const CubeTitle: React.FC<{ cubeId: string }> = ({ cubeId }) => {
  const [cubeTitle, setCubeTitle] = useState<string>(cubeId);
  useEffect(() => {
    const sub = Api.cube(cubeId).subscribe(cube => setCubeTitle(cube.cTitle));
    return () => sub.unsubscribe();
  }, [cubeId])

  return <span>{cubeTitle}</span>

}

const ArchetypeC: React.FC<{archetype: Archetype, cls: string}> = ({archetype, cls}) => {
  switch (archetype) {
    case "HorizontalBarChart":
      return <TfiBarChart className={cls + " rotate-90"}/>
    case "VerticalBarChart":
      return <TfiBarChart className={cls  }/>
    case "Heatmap":
      return <AiOutlineHeatMap className={cls}/>;
    case "Linechart":
      return <AiOutlineLineChart className={cls}/>
    case "Scatterplot":
      return <AiOutlineDotChart className={cls}/>
    default:
      return <HiViewGrid className={cls}/>
  }
}

function measurementIcon(m: Measurement): ReactNode {
  const cls = "w-4 h-4 ml-1"
  switch (m) {
    case "Nominal": return <TfiText className={cls}/>
    case "Ordinal": return <TfiDownload className={cls}/>
    case "Quantitative": return <TfiDashboard className={cls}/>
    case "TemporalRel": return <TfiTime className={cls}/>
    case "TemporalAbs": return <TfiCalendar className={cls}/>
    default:
      return <div></div>
  }
}

function backgroundAndBorder(isActive: boolean): string {
  return "cursor-pointer hover:bg-eucalyptus-200 " + (isActive ? "bg-eucalyptus-100 border border-slate-200" : "bg-slate-100 border border-slate-200")
}

const FieldRow: React.FC<{field: Field, currentSelection: Observable<Selections<Field>>, selChange: Subject<Change<Selections<Field>>>}> = ({field, currentSelection, selChange}) => {
  const [isActive, setIsActive] = useState<boolean>(false);

  const bld = isActive ? "text-gray-800" : "text-gray-500"

  const liClass = `flex flex-row rounded cursor-pointer h-12 align-middle items-center m-1 p-1 ${backgroundAndBorder(isActive)}`

  useEffect(() => {
    const sub = currentSelection.subscribe(sel => setIsActive(sel._WildCards.findIndex(f => f.name == field.name) >= 0));
    return () => sub.unsubscribe();
  }, [])

  function toggle(): void {
    selChange.next(_WildCardsL.modify((a: Field[]) => isActive ? a.filter(f => f.name != field.name) : a.concat([field])));
  }

  return <li className={liClass} onClick={toggle}>
    {measurementIcon(field.fieldType)}
    <span className={"ml-2 text-sm " + bld}>{field.name}</span>
    </li>;
}

export type Change<F> = (f: F) => F

const SelectionsComp: React.FC<{currentSelection: Observable<Selections<Field>>, selChange: Subject<Change<Selections<Field>>>, fields: Observable<Field[]> }> = ({currentSelection, selChange, fields}) => {
  const [allFields, setFields] = useState<Field[]>([]);

  useEffect(() => {
    const sub = fields.subscribe(setFields);
    return () => sub.unsubscribe();
  }, [])

  return <ul className="flex flex-col">{allFields.map(f => <FieldRow field={f} currentSelection={currentSelection} selChange={selChange}/>)}</ul>
}

const ChartTypeRow: React.FC<{chartType: Archetype, changes: Subject<Change<Selections<Field>>>, selections: Observable<Selections<Field>> }> = ({chartType, changes, selections}) => {
  const [isActive, setIsActive] = useState<boolean>(false);
  const itemClass = `w-12 h-12 m-1 p-1 flex items-center align-middle ${backgroundAndBorder(isActive)}`

  useEffect(() => {
    const sub = selections.subscribe(sel => setIsActive(sel._selectedArchetype.findIndex(f => f == chartType) >= 0));
    return () => sub.unsubscribe();
  }, [])

  function toggle(): void {
    changes.next(_ArchetypeL.modify((a: Archetype[]) => isActive ? a.filter(f => f != chartType) : a.concat([chartType])));
  }

  return <div onClick={toggle} className={itemClass}>
    <ArchetypeC archetype={chartType} cls="h-8 w-8" />
  </div>
}

const ChartTypeSelection: React.FC<{ changes: Subject<Change<Selections<Field>>>, selections: Observable<Selections<Field>> }> = ({changes, selections}) => {
  const allTypes: Archetype[] = ["Heatmap", "HorizontalBarChart", "Linechart", "Scatterplot", "VerticalBarChart", "Misc"]

  return <div className="flex flex-col mb-4 bg-slate-50 px-2">
            <span className="my-3 text-l font-medium">Chart type</span>
            <div className="flex flex-row flex-wrap flex-1">
              {allTypes.map(tp => <ChartTypeRow chartType={tp} changes={changes} selections={selections} />)}
            </div>
          </div>
}

const emptySelections: Selections<Field> = { _WildCards: [], _Color: [], _selectedArchetype: [], _selectedMark: [], _XAxis: [], _YAxis: [] }

const _WildCardsL: Lens<Selections<Field>, Field[]> = Lens.fromProp<Selections<Field>>()('_WildCards');
const _ArchetypeL: Lens<Selections<Field>, Archetype[]> = Lens.fromProp<Selections<Field>>()('_selectedArchetype')

const VisCard: React.FC<{vis: Visualisation}> = ({vis}) => {

  const [dt, setDt] = useState<any[]>([]);

  useEffect(() => {
    const sub = Api.evl({q: vis.visQuery, fields: vis.visFieldNames}).subscribe(setDt);

    return () => sub.unsubscribe();
  }, [vis])

  return <VegaLite style={{height: "500px"}} className="w-full border border-slate-300 mb-8" spec={vis.visDefinition} actions={false} data={{ table: dt }} />
}

const CubePage: React.FC<{ cubeId: string }> = ({ cubeId }) => {
  const [availableVisualisations, setAvailableVisualisations] = useState<Visualisation[]>([]);
  const [selectedVisualisation] = useState<Subject<Visualisation>>(new Subject<Visualisation>());
  const [selChange] = useState<Subject<Change<Selections<Field>>>>(new Subject());
  const [selections] = useState<Observable<Selections<Field>>>(
    selChange.pipe(
      scan((acc, f) => f(acc), emptySelections),
      startWith(emptySelections)
    )
  );
  const [cubeName, setCubeName] = useState<string>(cubeId);
  const [cube] = useState<Observable<Cube>>(Api.cube(cubeId));

  useEffect(() => {
    const sub = 
      cube.pipe(
        map(cube => cube.cQuery),
        mergeMap(query => selections.pipe(map<Selections<Field>, [string, Selections<Field>]>(selections => ([query, selections])))),
        mergeMap(([q, selections]) => Api.vis({q, selections}))
      ).subscribe(x => {
        setAvailableVisualisations(x);
        if (x.length > 0) { selectedVisualisation.next(x[0]) }
      });
    const sub3 = selectedVisualisation.subscribe(console.log);

    const sub4 = cube.subscribe(x => setCubeName(x.cTitle));

    return () => { sub.unsubscribe(); sub3.unsubscribe(); sub4.unsubscribe(); }
  }, [cubeId]);

  function dimSelection(name:string, p: (f: Field) => boolean): ReactNode {
    return <div className="flex flex-col mb-4 bg-slate-50 px-2">
            <span className="my-3 text-l font-medium">{name}</span>
            <SelectionsComp currentSelection={selections} fields={cube.pipe(map(c => c.cFields.filter(p)))} selChange={selChange} />
          </div>
  }

  const breadcrumbs = <div className="flex flex-row gap-1 text-slate-400 align-middle items-center justify-start h-8">
    <Link to="/"><span>Home</span></Link>
    <HiChevronRight/>
    <Link to="/cubes"><span>Cubes</span></Link>
    <HiChevronRight/>
    <Link to={`/cubes/${cubeId}`}><span className="text-slate-300">{cubeName}</span></Link>
  </div>

  return <Page navs={[]} title={<CubeTitle cubeId={cubeId} />} top={breadcrumbs}>
    <div className="flex flex-row flex-0 h-full">
      <div className="flex flex-col min-w-1/5 w-1/5 bg-slate-50 border border-r-slate-200">
        {dimSelection("Dimensions", f => f.fieldType != "Quantitative")}
        {dimSelection("Measures", f => f.fieldType == "Quantitative")}
        <ChartTypeSelection changes={selChange} selections={selections} />
      </div>
      <div style={{height: "98%"}} className="grid m-4 gap-2 flex-grow bottom-0 overflow-y-auto">
        {availableVisualisations.map(vis => <VisCard vis={vis}/>)}
      </div>
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