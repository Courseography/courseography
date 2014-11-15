DROP TABLE IF EXISTS Courses CASCADE;
DROP TABLE IF EXISTS Lectures CASCADE;
DROP TABLE IF EXISTS Tutorials CASCADE;
DROP TABLE IF EXISTS Breadth CASCADE;

CREATE TABLE Courses (
    Department varchar(3),
    cNum int,
    breadth int,
    title text,
    description text,
    manualTutorialEnrolment boolean,
    manualPracticalEnrolment boolean, -- Added this in.
    prereqs text[],
    exclusions text[],
    distribution text,
    prep ???
);

CREATE TABLE Tutorials (
    Department varchar(3),
    cNum int,
    tId varchar(5),
    times integer[][]
);

CREATE TABLE Lectures (
    Department varchar(3),
    cNum int,
    session varchar(1),
    lId varchar(5),
    times integer[][],
    capacity int,
    enrolled int,
    waitlist int,
    extra int,
    location varchar(10)
    time_str varchar(10), -- Doesn't need to be 10.


);

CREATE TABLE Breadth (
    bId int,
    description text
);