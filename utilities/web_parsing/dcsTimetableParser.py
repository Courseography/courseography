import glob
import csv
import json
import xlrd
from ttparser import TimetableParser
from faculty import facultyWebsites

outputDir = '../../res/'
coursePath = outputDir + 'courses/'
timetablePath = outputDir + 'timetable2014.csv'
timetableOutputPath = outputDir + 'timetable.html'

SHOW_ENROLMENT = True

data_map = {
    'code': 0,
    'session': 1,
    'section': 3,
    'time': 4,
    'kind': 5,
    'instructor': 6,
    'cap': 7
}


##################################################
# WORK WITH EXCEL FILE
##################################################

def generateCSV(path):
    '''
    Take the Excel spreadsheet and generate a csv from it
    '''

    book = xlrd.open_workbook(path)
    sh = book.sheet_by_index(0)
    with open(timetablePath, 'w+', newline='') as csvfile:
        for row in range(2, sh.nrows):
            if (sh.cell_value(rowx=row, colx=6) and
               sh.cell_value(rowx=row, colx=4) != 'not offered'):
                cols = []
                for col in [1, 2, 3, 4, 6, 7, 8, 11]:
                    val = sh.cell_value(rowx=row, colx=col)
                    if col == 11:
                        if val:
                            val = int(val)
                        cols.append(sanitize(val))
                    elif col == 1:
                        cols.append(sanitize(val)[:8])
                    else:
                        cols.append(sanitize(val))
                if len(''.join(cols).strip()) > 0:
                    csv.writer(csvfile, dialect='excel').writerow(cols)


def sanitize(s):
    ''' Really only for the Borodin/Boutillier cell '''
    return str(s).strip().replace('\n', '/')


def parse_dcs_timetable(f):
    parser = TimetableParser(lambda: generateCSV(f),
                             data_map,
                             'timetable2014.csv',
                             lambda x: x[data_map['section']] == 'not offered')
    return parser.run()

##################################################
# GENERATE HTML
##################################################


# Note: only returns courses that should appear in timetable
def getCSCcourses():
    courses = []
    for filename in glob.glob(coursePath + 'CSC*.txt'):
        print(filename)
        with open(filename, 'r', encoding='utf-8') as course_file:
            course_json = json.load(course_file)
            if 'F' in course_json or 'S' in course_json or 'Y' in course_json:
                courses.append(course_json)
    courses.sort(key=lambda course: course['name'])
    return courses


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
                    tutString = tutString.format('(' + tuts[i][1] + ')')
                else:
                    tutString = tutString.format('')

                extra = ' (+{})'.format(lec['extra']) if lec['extra'] > 0 else ''

                # TODO: fix hack for Borodin/Boutillier
                if lec['instructor'] == 'Borodin/Boutilier':
                    instructorString = '<a href="{}" target="_blank">{}</a>/'.format(
                                        facultyWebsites['Borodin'],
                                        'Borodin')
                    instructorString += '<a href="{}" target="_blank">{}</a>/'.format(
                                        facultyWebsites['Boutilier'],
                                        'Boutilier')
                elif lec['instructor'] in facultyWebsites:
                    instructorString = '<a href="{}" target="_blank">{}</a>'.format(
                                        facultyWebsites[lec['instructor']],
                                        lec['instructor'])
                else:
                    print('Could not find instructor ' + lec['instructor'])
                    instructorString = lec['instructor']

                # Enrolment
                if SHOW_ENROLMENT and 'enrol' in lec:
                    enrolString = '{}/{}'.format(lec['enrol'], lec['cap'])
                    if lec['enrol'] >= lec['cap']:
                        enrolString = '<strong>' + enrolString + '</strong>'
                else:
                    enrolString = str(lec['cap'])

                termRows.append(('<tr>' +
                                 '<td class="timetableSection">{}</td>' +
                                 '<td class="timetableTime">{} {}</td>' +
                                 '<td class="timetableInstructor">{}</td>' +
                                 '<td class="timetableCap">{}{}</td>' +
                                 '<td class="timetableWait">{}</td></tr>')
                                .format(lec['section'],
                                        lec['time_str'],
                                        tutString,
                                        instructorString,
                                        enrolString,
                                        extra,
                                        lec['wait'] if 'wait' in lec else 0
                                        ))

            # Add separate tutorial sections, if necessary
            if course['manualTutorialEnrolment']:
                for tut in tuts:
                    termRows.append('<tr><td class="timetableSection">' +
                                    tut[0] + '</td>' +
                                    '<td class="timetableTime">' +
                                    tut[2] + '</td></tr>')

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


def generateHTML(courses):
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
                         '<th class="timetableCap">Cap</th>' +
                         '<th class="timetableWait">Wait</th>' +
                         '</tr></table></td>' +
                         '<td class="SOffering"><table class="courseTable">' +
                         '<tr><th class="timetableSection">Sec</th>' +
                         '<th class="timetableTime">Time</th>' +
                         '<th class="timetableInstructor">Instructor</th>' +
                         '<th class="timetableCap">Cap</th>' +
                         '<th class="timetableWait">Wait</th>' +
                         '</tr></table></td>'
                         )

        for course in courses:
            htmlOutput.writelines(iter(generateRows(course)))
        htmlOutput.write('</table>')

#if __name__ == '__main__':
    #courses = parse_dcs_timetable('master.xlsx')
    #generateHTML(courses)
