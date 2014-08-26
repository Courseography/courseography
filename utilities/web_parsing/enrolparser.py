import urllib.request
import re
import json
import sys

SITE = 'http://ofr.artsci.utoronto.ca/enrolstats/{}.txt'
ENROL_REGEX = r'([A-Z]{3})\s*([A-Z]{3}[0-9]{3}[HY][015])\s*([FSY])(.*)((?:LEC|TUT|PRA))\s*([0-9]{4}).*?([0-9]{4})\s*([0-9]{4}).*\x00?.*([0-9]{10})'


def get_enrolment(course_dir, date, user, pwd):
    auth = urllib.request.HTTPBasicAuthHandler()
    auth.add_password(realm='Office of the Faculty Registrar, Arts and Science',
                      uri=SITE.format(date),
                      user=user,
                      passwd=pwd)
    opener = urllib.request.build_opener(auth)
    urllib.request.install_opener(opener)
    try:
        p = urllib.request.urlopen(SITE.format(date))
        enrol_str = p.read().decode('utf-8')

        results = re.findall(ENROL_REGEX, enrol_str, re.MULTILINE)
        with open('enrol.txt', 'w+') as f:
            for result in results:
                f.write('{},{},{},{},{},{},{}\n'.format(result[1],
                                                  result[2],
                                                  result[4],
                                                  result[5],
                                                  int(result[6]),
                                                  int(result[7]),
                                                  int(result[8])))

        for result in results:
            try:
                with open(course_dir + result[1] + '.txt', 'r', encoding='utf-8') as f:
                    try:
                        data = json.load(f)
                    except ValueError:
                        print('No course JSON', result[1])
                        continue

                try:
                    sections = data[result[2]]
                except KeyError:
                    print('KeyError', result[1], result[2])
                    continue

                if result[4][0] == 'L':
                    for lecture in sections['lectures']:
                        if lecture['section'] == 'L' + result[5]:
                            lecture['cap'] = int(result[6])
                            lecture['enrol'] = int(result[7])
                            lecture['wait'] = int(result[8])
                            break
                else:
                    for tutorial in sections['tutorials']:
                        if tutorial[0] == result[4][0] + result[5]:
                            if (len(tutorial) < 5):
                                tutorial.append(int(result[6]))
                                tutorial.append(int(result[7]))
                                tutorial.append(int(result[8]))
                            else:
                                tutorial[3] = int(result[6])
                                tutorial[4] = int(result[7])
                                if len(tutorial) < 6:
                                    tutorial.append(int(result[8]))
                                else:
                                    tutorial[5] = int(result[8])
                            break

                with open(course_dir + result[1] + '.txt', 'w+', encoding='utf-8') as f:
                    json.dump(data, f)

            except IOError:
                print('File not found', course_dir + result[1] + '.txt')

    except IOError as e:
        print(e.headers)

if __name__ == '__main__':
    # Need args directory, date, username, password
    if len(sys.argv) < 5:
        print('4 arguments required: dir, date, username, pwd')
    else:
        course_dir = sys.argv[1]
        date = sys.argv[2]
        user = sys.argv[3]
        pwd = sys.argv[4]
        get_enrolment(course_dir, date, user, pwd)
