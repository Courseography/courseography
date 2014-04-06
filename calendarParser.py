import json
import re
import urllib.request
from html.parser import HTMLParser

courses = []

# class CalendarParser(HTMLParser):
#   def __init__(self):
#     HTMLParser.__init__(self)
#     self.text = []
#   def handle_data(self, data):
#     if not data.isspace():
#       self.text.append(data)

# def downloadCalendar():
#   page = urllib.request.urlopen('http://www.artsandscience.utoronto.ca/ofr/calendar/crs_csc.htm')
#   parser = CalendarParser()
#   text = str(page.read())
#   parser.feed(text)
#   #print(page.read())
#   print(parser.text)

def parseCalendar():
  with open('res/calendar.txt', 'r') as calendarFile:
    course = {}
    for line in calendarFile:
      if line.startswith('CSC') or line.startswith('ECE') or line.startswith('MAT') or line.startswith('STA'):

        if (len(course) > 0):
          with open('res/courses/' + course['code'] + '.txt', 'w+') as output:
            json.dump(course, output)
            courses.append(course)

        titleParser = re.compile('(.{8})\s*(.*)\[')
        result = titleParser.match(line)
        course = {};
        course['description'] = ''
        course['code'] = result.group(1)
        course['title'] = result.group(2)
      elif line.startswith('Exclusion:'):
        course['exclusions'] = re.split(',|;', line[10:].rstrip('. \r\n'))
      elif line.startswith('Distribution Requirement Status: '):
        distStringLength = len('Distribution Requirement Status: ')
        course['distribution'] = line.strip()[distStringLength:]
      elif line.startswith('Breadth Requirement: '):
        breadthStringLength = len('Breadth Requirement: ')
        course['breadth'] = line.strip()[breadthStringLength:]
      elif line.startswith('Recommended Preparation: '):
        prepStringLength = len('Recommended Preparation: ')
        course['prep'] = line.strip()[prepStringLength:]
      elif line.startswith('Suggested preparation: '):
        prepStringLength = len('Suggested preparation: ')
        course['prep'] = line.strip()[prepStringLength:]
      elif line.startswith('Prerequisite: '):
        prereqString = line[14:].rstrip('. \r\n')
        course['prereqString'] = prereqString
        # Prerequisites for engineering students
        engRegex = re.compile('(.*)(Prerequisite for Engineering students only: .*)')
        engMatch = engRegex.match(prereqString)
        if (engMatch):
          engPrereq = engMatch.group(2)
          prereqString = engMatch.group(1)
        (prereqs, rest) = parseAnd(prereqString)
        if (engMatch):
          course['prereqs'] = prereqs.append(engPrereq)
        else:
          course['prereqs'] = prereqs
        if len(rest) > 0:
          print('Didn\'t finish parsing: s = ' + prereqString)
          print(course['prereqs'])
      elif len(line.strip()) > 0:
        course['description'] = course['description'] + line

def parseAnd(s):
  curr = s
  andList = []
  while len(curr) > 0:
    if curr[0] == ',' or curr[0] == ';' or curr[0] == ' ':
      curr = curr[1:]
    else:
      (prereqs, newCurr) = parseOr(curr)
      if curr == newCurr:
        print ('Parsing failed for ' + s + '  with curr = ' + curr)
        break
      else:
        curr = newCurr
        andList.append(prereqs)
  return (andList, curr)


def parseOr(s):
  curr = s
  orList = []
  while len(curr) > 0 and curr[0] != ',' and curr[0] != ';':
    if curr[0] == '(':
      tmp = curr[1:curr.find(')')]
      (prereqs, extra) = parseSinglePrerequisite(tmp)
      orList.append(prereqs)
      curr = curr[curr.find(')') + 1:]
    elif curr[0] == ' ' or curr[0] == '/':
      curr = curr[1:]
    else:
      (prereq, newCurr) = parseSinglePrerequisite(curr)
      if curr == newCurr:
        print ('Parsing failed for ' + s + 'with curr = ' + curr)
        break
      else:
        curr = newCurr
        orList.append(prereq)
  if len(orList) == 1:
    orList = orList[0]

  return (orList, curr)

def parseSinglePrerequisite(s):
  (course, rest) = parseCourse(s)
  if course == '':
    # Give up on parsing courses, and just get the string
    print('Gave up on:\n' + s + '\n')
    return (s, '')
  else:
    (extra, rest) = parseExtra(rest)
    if (len(extra) > 0):
      return ([course, extra], rest)
    else:
      return (course, rest)

def parseCourse(s):
  courseRegex = re.compile('[A-Z]{3}[\d]{3}[HY][\d]')
  courseMatch = courseRegex.match(s)
  if (courseMatch):
    return (courseMatch.group(0), s[len(courseMatch.group(0)):])
  else:
    return ('', s)

def parseExtra(s):
  extraRegex = re.compile('[\w \+%\-\(\)]*')
  if s.startswith('proficiency'):
    return (s, '')
  else:
    extraMatch = extraRegex.match(s)
    return (extraMatch.group(0), s[len(extraMatch.group(0)):])

if __name__ == '__main__':
  #downloadCalendar()
  parseCalendar()
  #print(parseOr('CSC200Y1blahbla/CSC165H1'))
  #print(parseAnd('CSC200Y1blahbla/CSC165H1'))
  #print(parseAnd('CSC209H1/proficiency in C++, Java, or Python;'))
  #print(parseAnd('STA247H1/STA255H1/STA257H1 or familiarity with basic probability theory; CSC209H1/proficiency in C++, Java, or Python;'))
