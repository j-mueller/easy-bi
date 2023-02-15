import React, { ReactNode } from "react";
import { Link, useMatch, useResolvedPath } from "react-router-dom";

export type PageTabLink = { name: string, linkTo: string, fullMatch: boolean}

const PageTab: React.FC<{link: PageTabLink}> = ({link}) => {
    const resolved = useResolvedPath(link.linkTo);
    const match = useMatch({ path: resolved.pathname, end: link.fullMatch });
    const classPrefix = (match) ? "border-t-2 border-gray-200 bg-gray-100" : "border-t-2 border-gray-100 hover:border-gray-200 hover:bg-gray-100"
    const className = classPrefix + " whitespace-nowrap pb-4 pt-2 px-2 font-medium text-sm"
    return <Link
                to={link.linkTo}
                key={link.linkTo}
                className={className}
                >{link.name}</Link>
}

const Page: React.FC<{title?: JSX.Element, navs: PageTabLink[], action?: JSX.Element, meta?: JSX.Element, children?: ReactNode}> = ({children, title, navs, meta, action}) => {
    return <div className="flex flex-col grow bg-gray-50 h-full">
        <div className="mt-2 md:flex md:items-center md:justify-between select-none p-5">
            <div className="flex-1 min-w-0">
                <h2 className="text-2xl font-bold leading-7 text-gray-900 sm:text-3xl sm:truncate">{title}</h2>
                <div className="mt-1 flex flex-col sm:flex-row sm:flex-wrap sm:mt-0 sm:space-x-6">
                    {meta}
                </div>
            </div>
            <div className="mt-4 flex-shrink-0 flex md:mt-0 md:ml-4">
                {action}
            </div>
        </div>
        <div className="border-b-2 border-gray-200 select-none">
            <div className="block">
                <nav className="-mb-px flex space-x-4">
                    {navs.map(nav => {return <PageTab link={nav}/>})}
                </nav>
            </div>
        </div>
            {children}
    </div>
}

export default Page;