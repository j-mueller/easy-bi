import React, { ReactNode, useEffect, useState } from "react";
import Api, { Archetype, Cube, InField, FieldGroup, Measurement, OutField, Selections, SortOrder, Visualisation, WithHash } from "../Api";
import { Link, useParams } from "react-router-dom";
import Page from "./Page";
import { map, mergeMap, Observable, scan, startWith, Subject } from "rxjs";
import { VegaLite } from "react-vega";
import { HiViewGrid, HiChevronRight } from "react-icons/hi";
import { AiOutlineDotChart, AiOutlineHeatMap, AiOutlineLineChart } from "react-icons/ai";
import { TfiBarChart, TfiCalendar, TfiDashboard, TfiDownload, TfiText, TfiTime } from "react-icons/tfi";
import { Lens } from 'monocle-ts';

const CubeTitle: React.FC<{ cubeId: string }> = ({ cubeId }) => {
  const [cubeTitle, setCubeTitle] = useState<string>(cubeId);
  useEffect(() => {
    const sub = Api.cube(cubeId).subscribe(cube => setCubeTitle(cube.name));
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
  const cls = "w-5 h-5"
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

function border(isActive: boolean): string {
  return isActive ? "border border-slate-400" : "border border-slate-200"
}

function backgroundAndBorder(isActive: boolean): string {
  return isActive ? "bg-eucalyptus-300 text-white border border-slate-200" : "bg-slate-100 text-slate-400 border border-slate-200";
}

const FieldGroupRow: React.FC<{fieldGroup: FieldGroup, currentSelection: Observable<Selections<InField>>, selChange: Subject<Change<Selections<InField>>> }> = ({fieldGroup, currentSelection, selChange}) => {
  return <li>
    <div className="flex flex-col">
    <span>{fieldGroup.name}</span>
    <ul>
      <FieldRow field={fieldGroup.primary_field} currentSelection={currentSelection} selChange={selChange} />
      {fieldGroup.other_fields.map(field => <FieldRow field={field} currentSelection={currentSelection} selChange={selChange} />)}
    </ul>
    </div>
  </li>
}

const FieldRow: React.FC<{field: OutField, currentSelection: Observable<Selections<InField>>, selChange: Subject<Change<Selections<InField>>>}> = ({field, currentSelection, selChange}) => {
  const [isActive, setIsActive] = useState<boolean>(false);
  const [isExpanded, setIsExpanded] = useState<boolean>(false);
  const [currentVal, setCurrentVal] = useState<InField>(Api.mkInField(field));

  useEffect(() => {
    const sub = currentSelection.subscribe(sel => setIsActive(sel._WildCards.findIndex(f => f.sql_field_name == field.sql_field_name) >= 0));
    const sub2 = currentSelection.pipe(map(x => x._WildCards)).subscribe(fields =>  setCurrentVal(fields.find(x => x.sql_field_name === field.sql_field_name) || Api.mkInField(field)));
    
    return () => { sub.unsubscribe(); sub2.unsubscribe};
  }, [])

  function toggle(): void {
    selChange.next(_WildCardsL.modify((a: InField[]) => isActive ? a.filter(f => f.sql_field_name != field.sql_field_name) : a.concat([Api.mkInField(field)])));
  }

  function rotateSortOrder(): void {
    selChange.next(_WildCardsL.modify((a: InField[]) => a.map(f => f.sql_field_name === field.sql_field_name ? _SortOrder.modify(nextOrder)(f) : f)));
  }

  function nextOrder(so: SortOrder): SortOrder {
    switch (so) {
      case "Ascending": return "Descending";
      case "Descending": return "None";
      default:
        return "Ascending";
    }
  }

  const exp = <>
    <div className="mx-2 flex flew-row gap-2 text-base">
      <span >Sort order:</span>
      <span onClick={rotateSortOrder} className="bg-slate-100 px-2 cursor-pointer">{currentVal.sort_order}</span>
    </div>
    </>;

  return <li className={`flex flex-col rounded m-1 p-1 ${border(isActive)} hover:bg-slate-200 ${isActive ? "bg-slate-200" : ""}`} key={field.sql_field_name}>
    <div className="flex flex-row h-12 items-center">
      <div className={`flex-grow flex flex-row items-center cursor-pointer`} onClick={toggle}>
        <div className={`${backgroundAndBorder(isActive)} h-10 w-10 flex flex-row justify-center items-center`}>
          {measurementIcon("Nominal")}
        </div>
        <span className={"ml-2 text-s text-slate-800"}>{field.display_name ? field.display_name : field.sql_field_name}</span>
      </div>
      <button className="p-2 cursor-pointer text-slate-400 hover:text-slate-800" onClick={() => setIsExpanded(!isExpanded)}>{isExpanded ? "-" : "+"}</button>
    </div>
    {isExpanded ? exp : <></>}
    </li>;
}

export type Change<F> = (f: F) => F

const SelectionsComp: React.FC<{currentSelection: Observable<Selections<InField>>, selChange: Subject<Change<Selections<InField>>>, fields: Observable<FieldGroup[]> }> = ({currentSelection, selChange, fields}) => {
  const [allFields, setFields] = useState<FieldGroup[]>([]);

  useEffect(() => {
    const sub = fields.subscribe(setFields);
    return () => sub.unsubscribe();
  }, [])

  return <ul className="flex flex-col">{allFields.map(f => <FieldGroupRow fieldGroup={f} currentSelection={currentSelection} selChange={selChange}/>)}</ul>
}

const ChartTypeRow: React.FC<{chartType: Archetype, changes: Subject<Change<Selections<InField>>>, selections: Observable<Selections<InField>> }> = ({chartType, changes, selections}) => {
  const [isActive, setIsActive] = useState<boolean>(false);
  const itemClass = `w-12 h-12 m-1 p-1 flex items-center align-middle ${backgroundAndBorder(isActive)}`

  useEffect(() => {
    const sub = selections.subscribe(sel => setIsActive(sel._selectedArchetype.findIndex(f => f == chartType) >= 0));
    return () => sub.unsubscribe();
  }, [])

  function toggle(): void {
    changes.next(_ArchetypeL.modify((a: Archetype[]) => isActive ? a.filter(f => f != chartType) : a.concat([chartType])));
  }

  return <div onClick={toggle} className={itemClass} key={chartType}>
    <ArchetypeC archetype={chartType} cls="h-8 w-8" />
  </div>
}

const ChartTypeSelection: React.FC<{ changes: Subject<Change<Selections<InField>>>, selections: Observable<Selections<InField>> }> = ({changes, selections}) => {
  const allTypes: Archetype[] = ["Heatmap", "HorizontalBarChart", "Linechart", "Scatterplot", "VerticalBarChart", "Misc"]

  return <div className="flex flex-col mb-4 bg-slate-50 px-2" key="chart-type-selection">
            <span className="my-3 text-l font-medium">Chart type</span>
            <div className="flex flex-row flex-wrap flex-1">
              {allTypes.map(tp => <ChartTypeRow chartType={tp} changes={changes} selections={selections} />)}
            </div>
          </div>
}

const emptySelections: Selections<InField> = { _WildCards: [], _Color: [], _selectedArchetype: [], _selectedMark: [], _XAxis: [], _YAxis: [] }

const _WildCardsL: Lens<Selections<InField>, InField[]> = Lens.fromProp<Selections<InField>>()('_WildCards');
const _ArchetypeL: Lens<Selections<InField>, Archetype[]> = Lens.fromProp<Selections<InField>>()('_selectedArchetype')
const _SortOrder: Lens<InField, SortOrder> = Lens.fromProp<InField>()('sort_order');

const VisCard: React.FC<{vis: WithHash<Visualisation>}> = ({vis}) => {

  const [dt, setDt] = useState<any[]>([]);

  function setData(newDT: any[]): void {
    // if set it to `newDT` right away, then the vegalite component
    // won't update. Therefore we first set an empty array,
    // and then the actual new value.
    setDt([]);
    setDt(newDT);
  }

  useEffect(() => {
    const sub = Api.evl({q: vis[1].visQuery, fields: vis[1].visFields}).subscribe(setData);

    return () => sub.unsubscribe();
  }, [vis[0]])

  return <VegaLite style={{height: "500px"}} className="w-full border border-slate-300 mb-8" spec={vis[1].visDefinition} actions={true} data={{table: dt}} key={vis[0]} />
}

const CubePage: React.FC<{ cubeId: string }> = ({ cubeId }) => {
  const [availableVisualisations, setAvailableVisualisations] = useState<WithHash<Visualisation>[]>([]);
  const [selectedVisualisation] = useState<Subject<WithHash<Visualisation>>>(new Subject<WithHash<Visualisation>>());
  const [selChange] = useState<Subject<Change<Selections<InField>>>>(new Subject());
  const [selections] = useState<Observable<Selections<InField>>>(
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
        map(cube => cube.query),
        mergeMap(query => selections.pipe(map<Selections<InField>, [string, Selections<InField>]>(selections => ([query, selections])))),
        mergeMap(([q, selections]) => Api.vis({q, selections}))
      ).subscribe(x => {
        setAvailableVisualisations(x);
        if (x.length > 0) { selectedVisualisation.next(x[0]) }
      });
    const sub3 = selectedVisualisation.subscribe(console.log);

    const sub4 = cube.subscribe(x => setCubeName(x.display_name));

    return () => { sub.unsubscribe(); sub3.unsubscribe(); sub4.unsubscribe(); }
  }, [cubeId]);

  function dimSelection(name:string, p: (f: FieldGroup) => boolean): ReactNode {
    return <div className="flex flex-col mb-4 bg-slate-50 px-2" key={name}>
            <span className="my-3 text-l font-medium">{name}</span>
            <SelectionsComp currentSelection={selections} fields={cube.pipe(map(c => c.fields.filter(p)))} selChange={selChange} />
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
      <div className="flex flex-col min-w-1/5 w-1/5 bg-slate-50 border border-r-slate-200 overflow-y-auto">
        {dimSelection("Dimensions", f => f.primary_field.field_options.tag != "quantitative")}
        {dimSelection("Measures", f => f.primary_field.field_options.tag == "quantitative")}
        <ChartTypeSelection changes={selChange} selections={selections} />
      </div>
      <div style={{height: "98%"}} className="grid m-4 gap-2 flex-grow bottom-0 overflow-y-auto" key="vis-cards">
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
