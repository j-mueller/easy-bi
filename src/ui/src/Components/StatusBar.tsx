import React from "react";

export type Status = "green" | "amber" | "red";

const Status: React.FC<{status: Status, text?: string }> = ({status, text}) => {
    return <div className="ae-status">
        <div className={"signal " + status}/>
        <div>{text}</div>
        </div>
}

export default Status;