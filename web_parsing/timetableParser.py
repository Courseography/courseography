import json
from grid import *
import xlrd
import sys

courses = []

outputDir = '../res/'
coursePath = outputDir + 'courses/'
timetablePath = outputDir + 'timetable2014.csv'
timetableOutputPath = outputDir + 'timetable.html'

fallGridPath = outputDir + 'fallGrid.html'
springGridPath = outputDir + 'springGrid.html'


class TimetableData:
    code = 0
    session = 1
    section = 3
    time = 4
    kind = 5
    instructor = 6
    cap = 7


##################################################
# WORK WITH EXCEL FILE
##################################################

def generateCSV(path):
    '''
    Take the Excel spreadsheet and generate a csv from it
    '''
    book = xlrd.open_workbook(path)
    sh = book.sheet_by_index(0)
    with open(timetablePath, 'w+') as csv:
        for row in range(2, sh.nrows):
            if (sh.cell_value(rowx=row, colx=6) and
               sh.cell_value(rowx=row, colx=4) != 'not offered'):
                for col in [1, 2, 3, 4, 6, 7, 8, 11]:
                    val = sh.cell_value(rowx=row, colx=col)
                    if col == 11:
                        if val:
                            val = int(val)
                        csv.write(sanitize(val))
                    else:
                        csv.write(sanitize(val) + ',')
                csv.write('\n')


def sanitize(s):
    ''' Really only for the Borodin/Boutillier cell '''
    return str(s).replace('\n', '/')


##################################################
# GENERATE MODEL
##################################################

def parseTimetable():
    '''
    Parse timetable from internal CSV (stored in timetablePath)
    Each line has the following fields:
    <Check Prereqs>, <Code>, <Session>, <Title>, <Section>, <Wait List>,
    <Time>, <Kind>, <Instructor>, <I>, <Controls>, <Large>, <Location>, <Notes>
    '''

    with open(timetablePath, 'r') as timetableFile:
        course = {}
        course['name'] = ''
        for line in timetableFile:
            data = line.split(',')
            code = data[TimetableData.code][:8]

            # Check if research project course
            if code and code[4] == '9' and code[5] != '0':
                continue

            # New course?
            if code and code != course['name']:
                # Save old course
                if course['name']:
                    courses.append(course)

                # Initialize new course
                course = addCourse(data)
                session = addSession(data, course)

            # New session?
            elif code and code == course['name']:
                session = addSession(data, course)

            # New section
            elif data[TimetableData.section]:
                addSection(data, course[session], course)
            else:
                addToSection(data, course[session])

        # Add last course
        courses.append(course)


def addCourse(data):
    return {
        'name': data[TimetableData.code][:8],
        'manualTutorialEnrolment': False
    }


def addSession(data, course):
    '''
    Adds a new session to the current course. Returns the name of the session.
    '''
    session = data[TimetableData.session]
    course[session] = {'lectures': [], 'tutorials': []}
    addSection(data, course[session], course)
    return session


def addSection(data, session, course):
    ''' Adds lecture/tutorial section; updates manualTutorialEnrolment. '''
    if data[TimetableData.section].startswith('L'):
        session['lectures'].append(makeLecture(data))

    if isTutorial(data):
        course['manualTutorialEnrolment'] = (data[TimetableData.section]
                                             .startswith('T'))
        session['tutorials'].append(makeTutorial(data))


def addToSection(data, session):
    '''
    Add extra meeting time (lec/tut) to an existing section.
    Called when data[TimetableData.section] is empty.
    '''
    if data[TimetableData.kind] == 'L':
        lecture = session['lectures'][-1]
        # Update time, cap, instructor
        if not lecture['time']:
            lecture['time'] = data[TimetableData.time]
        else:
            lecture['time'] += ', ' + data[TimetableData.time]
        if not lecture['cap'] or not lecture['cap'].strip().isdigit():
            lecture['cap'] = data[TimetableData.cap]
        if not lecture['instructor']:
            lecture['instructor'] = data[TimetableData.instructor]
    elif isTutorial(data):
        session['tutorials'].append(makeTutorial(data))
    else:
        print('Error on row', data)


def makeLecture(data):
    ''' Create a record of a lecture from a CSV line '''
    if data[TimetableData.section].startswith('L'):
        return {
            'section': data[TimetableData.section],
            'time': '' if isTutorial(data) else data[TimetableData.time],
            'cap': data[TimetableData.cap],
            'instructor': data[TimetableData.instructor],
            'extraCap': 0
        }
    else:
        print('makeLecture called incorrectly on row', data)


def makeTutorial(data):
    ''' Create a record of a tutorial from a CSV line '''
    if data[TimetableData.section].startswith('T'):
        return [data[TimetableData.section], data[TimetableData.time]]
    else:
        return data[TimetableData.time]


def isTutorial(data):
    ''' Returns true if data row represents a tutorial '''
    return data[TimetableData.kind] in ['T', 'LAB']


##################################################
# GENERATE HTML
##################################################

def generateRows(course):
    ''' Generate all html rows for one course. '''
    rows = []

    for term in ['Y', 'F', 'S']:
        if term in course:
            start = ('<td class="{}Offering" colspan="{}"">'
                     .format(term, 2 if term == 'Y' else 1)
                     + '<table class="courseTable" border>')
            rows.append(start)
            termRows = []

            lecs = course[term]['lectures']
            tuts = course[term]['tutorials']
            for i, lec, in enumerate(lecs):

                # Don't display enrolment control sections
                if lec['section'].startswith('L2'):
                    continue

                tutString = '<span style="float: right">{}</span>'
                if not course['manualTutorialEnrolment'] and i < len(tuts):
                    tutString = tutString.format('(' + tuts[i] + ')')
                else:
                    tutString = tutString.format('')

                extra = '+ ' + lec['extraCap'] if lec['extraCap'] > 0 else ''

                termRows.append(('<tr>' +
                                 '<td class="timetableSection">{}</td>' +
                                 '<td class="timetableTime">{} {}</td>' +
                                 '<td class="timetableInstructor">{}</td>' +
                                 '<td class="timetableCap">{}{}</td></tr>')
                                .format(lec['section'],
                                        lec['time'], tutString,
                                        lec['instructor'],
                                        lec['cap'],
                                        extra
                                        ))

            # Add separate tutorial secitons, if necessary
            if course['manualTutorialEnrolment']:
                for tut in tuts:
                    termRows.append('<tr><td class="timetableSection">' +
                                    tut[0] + '</td>' +
                                    '<td class="timetableTime">' +
                                    tut[1] + '</td></tr>')

            rows = (rows + termRows)
            rows.append('</table></td>')

            if term == 'Y':
                break

        # Section not offered that term
        elif term == 'F' or term == 'S':
            rows.append('<td class="{}Offering"></td>'.format(term))

    # TODO: remove inline styling
    rows = (['<tr class="searchClass">',
             '<td class="timetableCourseName"'
             + 'style="vertical-align:top">{}</td>'
             .format(course['name'])] + rows + ['</tr>'])

    return rows


def generateHTML():
    with open(timetableOutputPath, 'w+') as htmlOutput:
        htmlOutput.write('<table id="timetableMain" border><tr>' +
                         '<td class="timetableCourseName">Course</td>' +
                         '<th class="sessionHeader FOffering">FALL</th>' +
                         '<th class="sessionHeader SOffering">SPRING</th></tr>'
                         )

        # Header row
        htmlOutput.write('<tr><td class="timetableCourseName"></td>' +
                         '<td class="FOffering"><table class="courseTable">' +
                         '<tr><th class="timetableSection">Sec</th>' +
                         '<th class="timetableTime">Time</th>' +
                         '<th class="timetableInstructor">Instructor</th>' +
                         '<th class="timetableCap">Cap</th></tr></table></td>' +
                         '<td class="SOffering"><table class="courseTable">' +
                         '<tr><th class="timetableSection">Sec</th>' +
                         '<th class="timetableTime">Time</th>' +
                         '<th class="timetableInstructor">Instructor</th>' +
                         '<th class="timetableCap">Cap</th></tr></table></td>'
                         )

        for course in courses:
            htmlOutput.writelines(iter(generateRows(course)))
        htmlOutput.write('</table>')


##################################################
# GENERATE TIMETABLE GRID
##################################################

def generateFallGrid():
    generateGrid(['F', 'Y'], fallGridPath)


def generateSpringGrid():
    generateGrid(['S', 'Y'], springGridPath)


def generateGrid(terms, file):
    courseTimes = []
    for course in courses:
        for term in terms:
            if term in course:
                for lec in course[term]['lectures']:
                    if not lec['section'].startswith('L2') and lec['time']:
                        courseTimes.append((course['name'], lec['time']))

    grid = buildGrid(courseTimes)
    renderGrid(grid, file)


def write():
    for course in courses:
        name = course['name']
        path = coursePath + name + '.txt'
        with open(path, 'r', encoding='utf-8') as coursefile:
            data = json.load(coursefile)
            for term in ['F', 'S', 'Y']:
                if term in course:
                    data[term] = course[term]
                    for lec in data[term]['lectures']:
                        tmp = lec['time']
                        lec['time'] = parseTimeSlots(tmp)[0]
                    for i, tut in enumerate(data[term]['tutorials']):
                        if course['manualTutorialEnrolment']:
                            tut[1] = parseTimeSlots(tut[1])[0]
                        else:
                            data[term]['tutorials'][i] = parseTimeSlots(tut)[0]
            data['manualTutorialEnrolment'] = course['manualTutorialEnrolment']

        with open(path, 'w', encoding='utf-8') as coursefile:
            json.dump(data, coursefile)

if __name__ == '__main__':
    parseTimetable()
    generateHTML()
    write()
