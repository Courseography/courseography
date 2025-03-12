# [Courseography](https://courseography.teach.cs.toronto.edu/graph)

## About

Here at the University of Toronto, we have hundreds of courses to choose from, and it can be hard to navigate prerequisite chains, program requirements, and term-by-term offerings all at once. That's where Courseography comes in: by presenting course and scheduling information in a set of graphical interactive tools, we make it easier to choose the right courses for your academic career. Whether it's making sure you'll satisfy all the prerequities for that 4th year course you really want to take, or fitting together fragments of your schedule for next term, we hope Courseography makes your life easier!

Powered by [Haskell](https://www.haskell.org/), Courseography was started in late 2013 by [David Liu](http://www.cs.toronto.edu/~david/). However, it wasn't until he recruited [Ian Stewart-Binks](http://www.cs.toronto.edu/~iansb/) to the project that things really got rolling. Though the past two years have really seen our tools take off within the CS student body, there's still a long way for us to go. Our current projects include moving the front-end of the application over to [React](https://facebook.github.io/react/), unifying the graph viewing and drawing tools, and improved exporting and report generation.

## Getting Involved

See [CONTRIBUTING.md](https://github.com/Courseography/courseography/blob/master/CONTRIBUTING.md).

## Installation

#### Download Courseography

1. Run `$ git clone https://github.com/Courseography/courseography.git` to create a local copy of the Courseography.

#### Software Dependencies

First install the following:

1. [Haskell](https://www.haskell.org/ghcup/)
2. [Node.js](https://nodejs.org/en/download/) _v18.20.2_ (not v20+)
3. Yarn, by opening a terminal and running: `npm install -g yarn`
4. [ImageMagick](http://www.imagemagick.org/script/download.php)
5. [GraphViz](https://graphviz.org/download/)
6. [LaTeX](https://www.latex-project.org/get/)

Then, open a terminal window and `cd` into your `courseography` repository folder.
The remaining steps should all be run in this terminal window.

#### Other Files

_Run this step manually._

1. Create the db folder with `$ mkdir db`

#### Installing

Run the following commands (the parts after `#` are just comments):

```console
$ yarn install  # Install all Javascript dependencies
$ stack setup   # Install the required GHC compiler
$ stack build   # Compile Courseography and all Haskell dependencies (this will take a while)
```

#### Parsing and Generation

1. Create the database file `$ stack run database-setup`
2. Parse prerequisite graphs `$ stack run database-graphs`
3. Parse course information from Arts and Science Calendar `$ stack run database-calendar`
4. Parse course information from Arts and Science Timetable `$ stack run database-timetable`

#### Running

1. Run `$ yarn watch` to build the web static assets (Javascript and CSS files)
2. In a new terminal window Run `$ stack run` to start the server
3. Navigate to `http://localhost:8000/graph` in your browser

When running in production you should run `$ yarn build` instead of `$ yarn watch` to build the web assets.
This will take longer but results in smaller asset files.

#### Running front-end tests

To run all tests, run `$ yarn run test`. However, if you need to run a specific file or folder of tests,
run `$ yarn run test -- <path>`.

#### Developers

If you are contributing to Courseography, you should run the following to install and test our pre-commit hooks:

1. `yarn prepare`
2. `stack install hlint`
3. `npx lint-staged` (this will run the pre-commit hooks; make sure that it runs without errors)

Also follow these steps to get IDE like support for Haskell through [Haskell Language Server (HLS)](https://github.com/Courseography/courseography/wiki/Haskell-Language-Server-%28HLS%29-Guide)

1. Open VS Code.
2. Navigate to the **Extensions** marketplace (`Cmd+Shift+X` on macOS, `Ctrl+Shift+X` on Windows/Linux).
3. Search for **"Haskell"**.
4. Click **Install** on the extension provided by "Haskell (haskell.haskell)".

The extension will automatically download and configure Haskell Language Server.

**Note:** VS Code is the easiest way to to use HLS, but you can look at this [installation guide](https://haskell-language-server.readthedocs.io/en/latest/installation.html) if you want to use a different text editor.

There is a guide

## Contributors

This project would not exist without the contributions of many students in the Department of Computer Science. In alphabetical order, our contributors are:

Ismail Ahmed,
Alex Baluta,
Mehdi Benallegue,
Alexander Biggs,
Kelly Bell,
Ching Chang,
Christina Chen,
Eugene Cheung,
Mimis Chlympatsos,
Kael Deverell,
Spencer Elliott,
Lana El Sanyoura,
Ryan Fan,
Ailsa Fang,
Christian Garcia,
Ross Gatih,
Nazanin Ghazitabatabai,
Sidharth Gupta,
Parker Hutcheson,
Jai Joshi,
Philip Kukulak,
Jaeyong Lee,
Ryan Lee,
Tamara Lipowski,
Ethan Liu,
Lydia Liu,
Nathan Liu,
Siqi Liu,
Jahnavi Matholia,
Hermish Mehta,
Mia Meng,
Christine Murad,
Justin Park,
Harsh Patel,
Eleonora Scognamiglio,
Sam Shaftoe,
Ian Stewart-Binks,
Maryam Taj,
Betty Wang,
Fullchee Zhang,
Minfan Zhang,
Alex Shih,
Cassandra Stefura,
Zi Kai Xu

## Privacy Policy and Licensing

Click [here](https://github.com/Courseography/courseography/blob/master/PRIVACY.md) to learn about our Privacy Policy.

Click [here](https://github.com/Courseography/courseography/blob/master/LICENSE) to learn about our licensing.
