import React, { useEffect, useState } from 'react';
import { useParams } from 'react-router-dom';

const Chat = () => {
    const [connectionStatus, setConnectionStatus] = useState('idle');

    useEffect(() => {
        const urlParams = new URLSearchParams(window.location.search);
        const incoming = urlParams.get('incoming');
        const { user } = useParams();

        if (incoming === 'true') {
            // Step 1: Sending Connection Request ...
            setConnectionStatus('sending');

            // Step 2: After informing backend server, "Connection Request Sent. Waiting for user". Keep fetching status
            fetch('/api/connection-request')
                .then(response => response.json())
                .then(data => {
                    if (data.status === 'accepted') {
                        // Step 3: If connection is accepted, show chatbox
                        setConnectionStatus('accepted');
                    } else {
                        // Connection request is still pending
                        setTimeout(fetchConnectionStatus, 2000);
                    }
                })
                .catch(error => {
                    console.error('Error:', error);
                    setConnectionStatus('error');
                });
        }
    }, []);

    const fetchConnectionStatus = () => {
        fetch('/api/connection-status')
            .then(response => response.json())
            .then(data => {
                if (data.status === 'accepted') {
                    // Step 3: If connection is accepted, show chatbox
                    setConnectionStatus('accepted');
                } else {
                    // Connection request is still pending
                    setTimeout(fetchConnectionStatus, 2000);
                }
            })
            .catch(error => {
                console.error('Error:', error);
                setConnectionStatus('error');
            });
    };

    return (
        <div>
            {connectionStatus === 'sending' && <p>Sending Connection Request ...</p>}
            {connectionStatus === 'accepted' && <p>Show chatbox here</p>}
            {connectionStatus === 'error' && <p>Error occurred while connecting</p>}
        </div>
    );
};

export default Chat;