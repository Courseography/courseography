import dcsTimetableParser
import fasTimetableParser
import calendarParser
import fasCalendarParser
import argparse
import os
import shutil

COURSES_DIR = '../../res/courses'

if __name__ == '__main__':
  # Parse arguments
  parser = argparse.ArgumentParser(description='Build Course Planner.')
  parser.add_argument('master', help='master CSC spreadsheet file')
  parser.add_argument('--clean', action='store_const', const=True, help='remove existing files')
  parser.add_argument('--nofas', action='store_const', const=True, help='don\'t regenerate fas files')
  args = parser.parse_args()

  # Clean
  if args.clean:
    print('Removing directory ' + COURSES_DIR)
    shutil.rmtree(COURSES_DIR)

  # Make correct directory
  if not os.path.exists(COURSES_DIR):
    print('Creating directory ' + COURSES_DIR)
    os.makedirs(COURSES_DIR)

  # Create files for all FAS courses
  if not args.nofas:
    print('Creating FAS course files')
    fasTimetableParser.parse_fas_timetable()

  # Parse FAS calendar
  print('Fetching course titles')
  fasCalendarParser.parseFASCalendar()

  # Create CSC files from calendar info
  print('Extra calendar info for CSC courses')
  calendarParser.parseCalendar()

  # Add to CSC files using timetable
  print('Extra timetable info for CSC courses')
  courses = dcsTimetableParser.parse_dcs_timetable(args.master)
  dcsTimetableParser.generateHTML(courses)
