// TSX syntax highlighting test
import React, { useState, FC } from 'react';

interface GreetingProps {
    name: string;
    count?: number;
}

const Greeting: FC<GreetingProps> = ({ name, count = 0 }) => {
    const [clicks, setClicks] = useState(count);

    return (
        <div className="greeting">
            <h1>Hello, {name}!</h1>
            <p>Clicks: {clicks}</p>
            <button onClick={() => setClicks(c => c + 1)}>
                Click me
            </button>
        </div>
    );
};

export default Greeting;
