import React from 'react';
import ReactDOM from 'react-dom';

  // ========================================

  ReactDOM.render(
    <html>
    <body>
    <h1>Generate Prerequisites:</h1>
    <div>
    <ul>
    <li><p class="filter">Enter Course(s): </p>
    <input class="filter" placeholder="..."/></li>
    <li><p class="filter">Exclude Courses: </p>
    <input class="filter" placeholder="..."/></li>
    <li><p class="filter">Departments: </p>
    <input class="filter" placeholder="..."/>
    <ul>
      <li>
        <p class="filter">How many layers of courses from other department(s) do you want to include? </p>
        <input class="filter" placeholder="..."/>
      </li>
    </ul>
    </li>
    <li><p class="filter">Number of layers(?): </p>
    <input class="filter" placeholder="..."/></li> 
    <li><p class="filter">Faulties: </p>
    <input class="filter" placeholder="..."/></li>                           
    <li><p class="filter">Campuses: </p>
    <input class="filter" placeholder="..."/></li>
    <li><p class="filter">Exclude courses external to any university campus: </p>
    <input type="checkbox" class="filter"/></li>
    <li><p class="filter">Exclude grade requirements: </p>
    <input type="checkbox" class="filter"/></li>
    </ul>
    </div>
    </body>
    </html>,

    document.getElementById('generate-prerequisites')
  );