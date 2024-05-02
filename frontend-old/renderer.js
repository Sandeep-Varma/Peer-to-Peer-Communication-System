// // File: renderer.js

// // Fetch names from the server
// fetch('/names')
//     .then(response => response.json()) // Parse the response as JSON
//     .then(names => {
//         // Create a list element for each name
//         const listItems = names.map(name => {
//             const li = document.createElement('li');
//             li.textContent = name;
//             return li;
//         });

//         // Append the list items to the body of the document
//         const ul = document.createElement('ul');
//         listItems.forEach(li => ul.appendChild(li));
//         document.body.appendChild(ul);
//     })
//     .catch(error => console.error('Error:', error));



// File: renderer.js
const { ipcRenderer } = require('electron');

// Directly use the names list
const names = ["Sandeep", "Bhuvan"];

// Create a list element for each name
const listItems = names.map(name => {
    const li = document.createElement('li');
    li.textContent = name;

    // Add a click event listener to the list item
    li.addEventListener('click', () => {
        // Send a message to the main process to open a new window
        ipcRenderer.send('open-chat-window', name);
    });

    return li;
});

// Append the list items to the body of the document
const ul = document.createElement('ul');
listItems.forEach(li => ul.appendChild(li));
document.body.appendChild(ul);