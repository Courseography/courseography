import csv
import json
from grid import *

class TimetableParser:
    """ A class with the ability to parse a timetable.
    Written for input from both FAS and DCS.
    """

    RES_PATH = '../res/'
    COURSES_PATH = RES_PATH + 'courses/'

    def __init__(self, gen_data, data_map, data_file):
        self.gen_data = gen_data  # Function which generates a csv file
        self.data_file = data_file  # File name for csv
        self.data_map = data_map
        self.use_kind = 'kind' in data_map
        self.code = data_map['code']
        self.session = data_map['session']
        self.section = data_map['section']
        self.time = data_map['time']
        self.instructor = data_map['instructor']
        self.kind = data_map.get('kind', -1)
        self.cap = data_map.get('cap', -1)

        self.courses = []


    def run(self):
        self.gen_data()
        return self.parse_timetable()

    def parse_timetable(self):
        '''
        Parse timetable from csv at location RES_PATH + self.data_file.
        Fields in csv are specified by self.data_map.
        Returns a list of courses.
        Also writes courses to file.
        '''

        with open(TimetableParser.RES_PATH + self.data_file,
                  'r', encoding='utf-8') as time_file:
            course = {'name': ''}

            reader = csv.reader(time_file)
            for data in reader:
                # TODO: fix
                if len(data) <= max(self.data_map.values()):
                    try:
                        print('Skipping short line "{}"'.format(data))
                    except Exception:
                        print('Skipping short line, unicode :(')
                    continue

                code = data[self.code]

                # Check if code is not an actual course code
                if len(code) > 8:
                    print('Ignoring code:', code)
                    data[self.code] = ''
                    code = ''

                # New course?
                if code and code != course['name']:
                    # Save old course
                    if course['name']:
                        self.finalise_course(course)
                    # Initialize new course
                    course = self.add_course(data)
                    session = self.add_session(data, course)
                # New session?
                elif code:
                    session = self.add_session(data, course)
                # New section?
                elif data[self.section]:
                    self.add_section(data, course[session], course)
                # Extra time, existing section
                else:
                    self.add_to_section(data, course[session])

            # Add last course
            self.finalise_course(course)
            return self.courses

    def finalise_course(self, course):
        ''' Add course to the list of courses. Also handle reserved seats. '''

        # Check if not a research project course
        code = course['name']
        if code and not self.is_research_course(code):
            # Resolve extra caps
            for session in ['F', 'S', 'Y']:
                if session in course:
                    for lec in course[session]['lectures']:
                        if lec['section'].startswith('L2'):
                            for lec2 in course[session]['lectures']:
                                if lec['time'] == lec2['time']:
                                    lec2['extra'] += lec['cap']

        path = TimetableParser.COURSES_PATH + code + '.txt'

        try:
            with open(path, 'r', encoding='utf-8') as course_file:
                old = json.load(course_file)
                for field in ['title', 
                              'description', 
                              'exclusions', 
                              'distribution', 
                              'breadth', 
                              'prep', 
                              'prereqs', 
                              'prereqString']:
                    course[field] = old.get(field)
        except FileNotFoundError:
            pass
        with open(TimetableParser.COURSES_PATH + code + '.txt', 'w+') as output:
            json.dump(course, output)
        self.courses.append(course)


    def add_course(self, data):
        """ Make and return new course from data. """

        course = {'name': data[self.code],
                  'manualTutorialEnrolment': False
                  }
        return course


    def add_session(self, data, course):
        """ Add new session to course. """

        session = data[self.session]
        course[session] = {'lectures': [], 'tutorials': []}
        self.add_section(data, course[session], course)
        return session

    def add_section(self, data, session, course):
        """ Add lecture/tutorial section; update manualTutorialEnrolment. """

        if data[self.section].startswith('L'):
            session['lectures'].append(self.make_lecture(data))
        if self.is_tutorial(data):
            course['manualTutorialEnrolment'] = data[self.section][0] in 'PT'
            session['tutorials'].append(self.make_tutorial(data))


    def add_to_section(self, data, session):
        '''
        Add extra meeting time (lec/tut) to existing section.
        Called when data[self.section] is empty.
        '''

        if self.is_lecture(data):
            lecture = session['lectures'][-1]
            # Update instructor, time, cap
            time = data[self.time]
            lecture['time'] += parse_time_slots(time)[0]
            if lecture['time_str']:
                lecture['time_str'] += ', ' + time
            else:
                lecture['time_str'] = time
            lecture['instructor'] = lecture.get('instructor',
                                                data[self.instructor])
            if self.cap >= 0:
                lecture['cap'] += int(data[self.cap]) if data[self.cap] else 0
        elif self.is_tutorial(data):
            session['tutorials'].append(self.make_tutorial(data))
        else:
            try:
                print('Error on row ', data)
            except Exception:
                print('Error on row, unicode :(')


    def make_lecture(self, data):
        """ Create a lecture from data. """
        if data[self.section].startswith('L'):
            lec = {
                'section': data[self.section],
                'instructor': data[self.instructor],
                'extra': 0
            }
            time = data[self.time]
            if self.is_tutorial(data):
                lec['time'] = []
                lec['time_str'] = ''
            else:
                lec['time'] = parse_time_slots(time)[0]
                lec['time_str'] = time
            if self.cap >= 0 and data[self.cap]:
                lec['cap'] = int(data[self.cap])
            else:
                lec['cap'] = 0

            return lec
        else:
            print('make_lecture called incorrectly on row ', data)


    def make_tutorial(self, data):
        """ Create a tutorial from data. """

        if data[self.section].startswith(('T', 'P')):
            return [data[self.section], 
                    parse_time_slots(data[self.time])[0], 
                    data[self.time]]
        else:
            return [parse_time_slots(data[self.time])[0],
                    data[self.time]]


    def is_research_course(self, code):
        return len(code) == 8 and code[4] == '9' and code[5] not in '01'


    def is_tutorial(self, data):
        ''' Returns True if data represents a tutorial. '''

        if self.kind >= 0 and data[self.kind] in ['T', 'LAB']:
            return True

        section = data[self.section]
        return (section is None or len(section) == 0 or
               (len(section) == 5 and section.startswith(('P', 'T'))))


    def is_lecture(self, data):
        ''' Returns True if data represents a lecture. '''

        if self.kind >= 0 and data[self.kind] == 'L':
            return True

        section = data[self.section]
        return len(section) == 5 and section.startswith('L')
