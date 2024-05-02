import React, { useState, useEffect } from 'react';
import { useNavigate } from 'react-router-dom';

const Home = () => {
    const [username, setUsername] = useState('');
    const [connections, setConnections] = useState([]);
    const navigate = useNavigate();

    const handleUsernameChange = (event) => {
        setUsername(event.target.value);
    };

    const handleConnect = () => {
        // redirect to /chat/:username
        navigate(`/chat/${username}`);
    };

    useEffect(() => {
        // Fetch the list of usernames from the backend and update the 'connections' state variable
        // You can use the 'setConnections' function to update the state
    }, []);

    return (
        <div>
            <input type="text" value={username} onChange={handleUsernameChange} onKeyPress={(event) => event.key === 'Enter' && handleConnect()} />
            <button onClick={handleConnect}>Connect</button>
            <ul>
                {connections.map((connection) => (
                    <li key={connection}>{connection}</li>
                ))}
            </ul>
        </div>
    );
};

export default Home;