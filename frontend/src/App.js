import { BrowserRouter as Router, Route, Routes } from 'react-router-dom';
import DefaultRedirector from './default';
import Home from './comp/home';
import Chat from './comp/chat';

const App = ()=>{
  return (
    <>
    <Router forceRefresh={true}>
        <Routes>
          <Route exact path="/home" element={<Home/>}/>
          <Route exact path="/chat/:id" element={<Chat/>}/>
          <Route path ="*" element={<DefaultRedirector/>}/>
        </Routes>
    </Router>
    </>
  );
}

export default App;
