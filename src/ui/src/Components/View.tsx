import React, { ReactNode, useEffect, useState } from "react";
import Api, { Visualisation } from "../Api";
import { useParams } from "react-router-dom";
import Page from "./Page";
import { mergeMap } from "rxjs";

const ViewTitle: React.FC<{ viewId: string }> = ({ viewId }) => {
  const [viewTitle, setViewTitle] = useState<string>(viewId);
  useEffect(() => {
    const sub = Api.view(viewId).subscribe(view => setViewTitle(view.vTitle));
    return () => sub.unsubscribe();
  }, [viewId])

  return <span>{viewTitle}</span>

}

const ViewPage: React.FC<{ viewId: string }> = ({ viewId }) => {
  const [vis, setVis] = useState<Visualisation[]>([]);

  useEffect(() => {
    const sub = Api.view(viewId).pipe(
      mergeMap(view => Api.vis(view.vQuery))
    ).subscribe(setVis);
    return () => sub.unsubscribe();
  }, [viewId]);

  function mkVisRow(vis: Visualisation): ReactNode {
    return <li className="cursor-pointer w-32 m-2 bg-slate-300 hover:bg-eucalyptus-300 hover:border-eucalyptus-700 border-slate-700 border-2">{vis.visDescription}</li>;
  }

  return <Page navs={[]} title={<ViewTitle viewId={viewId} />}>
    <div className="flex flex-col flex-1">
    <div className="flex-grow">VISUALISATION</div>
    <ul className="flex flex-row h-32">{vis.map(mkVisRow)}</ul>
    </div>
  </Page>
}

const ViewComponent: React.FC = () => {
  const params = useParams();
  const viewId = params["viewid"] || "";
  if (viewId === "") { console.warn("ViewPage: View ID not found in URL params") };
  return <ViewPage viewId={viewId} />
}

export default ViewComponent;