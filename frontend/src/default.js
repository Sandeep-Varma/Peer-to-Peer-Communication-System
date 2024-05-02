import React, {useEffect} from 'react';
import { useNavigate } from 'react-router-dom';

const DefaultRedirector = ()=>{
    const navigate = useNavigate();
    
    useEffect(()=>{
        navigate("/home");
    }, [navigate]);

    return (
        <div>
            <h1>Redirecting ...</h1>
        </div>
    );
}

export default DefaultRedirector;