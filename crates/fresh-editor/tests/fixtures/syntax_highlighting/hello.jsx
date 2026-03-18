// JSX syntax highlighting test
import React, { useState } from 'react';

function Greeting({ name }) {
    const [count, setCount] = useState(0);

    return (
        <div className="greeting">
            <h1>Hello, {name}!</h1>
            <button onClick={() => setCount(count + 1)}>
                Clicked {count} times
            </button>
        </div>
    );
}

export default Greeting;
