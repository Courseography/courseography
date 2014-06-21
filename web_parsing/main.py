import timetableParser
import fasTimetableParser
import calendarParser
import fasCalendarParser
import argparse
import os
import shutil

if __name__ == '__main__':
  # Parse arguments
  parser = argparse.ArgumentParser(description='Build Course Planner.')
  parser.add_argument('master', help='master CSC spreadsheet file')
  parser.add_argument('--clean', action='store_const', const=True, help='remove existing files')
  parser.add_argument('--nofas', action='store_const', const=True, help='don\'t regenerate fas files')
  parser.add_argument('--grid', action='store_const', const=True, help='generate simple grid of courses')
  args = parser.parse_args()

  # Clean
  if args.clean:
    print('Removing directory ../res/courses')
    shutil.rmtree('../res/courses')

  # Make correct directory
  if not os.path.exists('../res/courses'):
    print('Creating directory ../res/courses')
    os.makedirs('../res/courses')

  # Create files for all FAS courses
  if not args.nofas:
    print('Creating FAS course files')
    fasTimetableParser.downloadTimetables()
    fasTimetableParser.parseTimetable()

  # Parse FAS calendar
  print('Fetching course titles')
  fasCalendarParser.parseFASCalendar()

  # Create CSC files from calendar info
  print('Extra calendar info for CSC courses')
  calendarParser.parseCalendar()

  # Add to CSC files using timetable
  print('Extra timetable info for CSC courses')
  timetableParser.generateCSV(args.master)
  timetableParser.parseTimetable()
  timetableParser.generateHTML()
  timetableParser.write()

  # Generate grids
  if args.grid:
    timetableParser.generateFallGrid()
    timetableParser.generateSpringGrid()