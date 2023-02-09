import React from "react";
import ReactDOM from "react-dom";

import './index.css';

const App = () => {
  return (
    <div>
        <div className="text-gray-700">Welcome to my-webpack-react-starter</div>
    </div>
  );
};

ReactDOM.render(<App />, document.querySelector("#root"));