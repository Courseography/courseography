import { CourseModal } from "../react_modal.js.jsx"
/**
 * Test for getTable.
 */
describe("getTable", () => {
  let actual
  let meetingTime
  let expected
  const wrapper = new CourseModal({})

  describe("The meeting time has only one lecture and the lecture has one occurence", () => {
    beforeEach(() => {
      meetingTime = [
        {
          meetData: {
            cap: 69,
            code: "CSC419H1",
            enrol: 38,
            extra: 0,
            instructor: "A. Jacobson",
            section: "LEC0101",
            session: "F",
            wait: 0,
          },
          timeData: [
            {
              endHour: 17,
              firstRoom: {
                address: "80  St. George Street",
                bCode: "LM",
                bName: "Lash Miller Chemical Laboratories",
                lat: 43.66160185,
                lng: -79.39841172598216,
                postalCode: "M5S 3H6",
                room: "LM 158",
              },
              secondRoom: null,
              startHour: 15,
              weekDay: 1,
            },
          ],
        },
      ]
    })
    test("The information of the lecture is formatted correctly", () => {
      actual = wrapper.getTable(meetingTime, "F")
      expected = [
        {
          activity: "LEC0101",
          instructor: "A. Jacobson",
          availability: "31 of 69 available",
          waitList: "0 students",
          time: ["Tuesday 15 - 17"],
          room: ["LM 158 "],
        },
      ]
      expect(actual).toEqual(expected)
    })
  })

  describe("The meeting time has one lecture with more than one occurences", () => {
    beforeEach(() => {
      meetingTime = [
        {
          meetData: {
            cap: 69,
            code: "CSC419H1",
            enrol: 38,
            extra: 0,
            instructor: "A. Jacobson",
            section: "LEC0101",
            session: "F",
            wait: 0,
          },
          timeData: [
            {
              endHour: 17,
              firstRoom: {
                address: "15  King's College Circle",
                bCode: "UC",
                bName: "University College",
                lat: 43.66287995,
                lng: -79.395181775127,
                postalCode: "M5S 3H7",
                room: "UC 179",
              },
              secondRoom: null,
              startHour: 16,
              weekDay: 3,
            },
            {
              endHour: 17,
              firstRoom: {
                address: "80  St. George Street",
                bCode: "LM",
                bName: "Lash Miller Chemical Laboratories",
                lat: 43.66160185,
                lng: -79.39841172598216,
                postalCode: "M5S 3H6",
                room: "LM 158",
              },
              secondRoom: null,
              startHour: 15,
              weekDay: 1,
            },
          ],
        },
      ]
    })
    test("The occurences of the lecture are sorted by day", () => {
      actual = wrapper.getTable(meetingTime, "F")
      expected = [
        {
          activity: "LEC0101",
          instructor: "A. Jacobson",
          availability: "31 of 69 available",
          waitList: "0 students",
          time: ["Tuesday 15 - 17", "Thursday 16 - 17"],
          room: ["LM 158 ", "UC 179 "],
        },
      ]

      expect(actual).toEqual(expected)
    })
  })

  describe("The occurence of the lecture has two rooms ", () => {
    beforeEach(() => {
      meetingTime = [
        {
          meetData: {
            cap: 69,
            code: "CSC419H1",
            enrol: 38,
            extra: 0,
            instructor: "A. Jacobson",
            section: "LEC0101",
            session: "F",
            wait: 0,
          },
          timeData: [
            {
              endHour: 17,
              firstRoom: {
                address: "80  St. George Street",
                bCode: "LM",
                bName: "Lash Miller Chemical Laboratories",
                lat: 43.66160185,
                lng: -79.39841172598216,
                postalCode: "M5S 3H6",
                room: "LM 158",
              },
              secondRoom: {
                address: "80  St. George Street",
                bCode: "LM",
                bName: "Lash Miller Chemical Laboratories",
                lat: 43.66160185,
                lng: -79.39841172598216,
                postalCode: "M5S 3H6",
                room: "UC 179",
              },
              startHour: 15,
              weekDay: 1,
            },
          ],
        },
      ]
    })
    test("Both of the first and second room of the occurence are shown", () => {
      actual = wrapper.getTable(meetingTime, "F")
      expected = [
        {
          activity: "LEC0101",
          instructor: "A. Jacobson",
          availability: "31 of 69 available",
          waitList: "0 students",
          time: ["Tuesday 15 - 17"],
          room: ["LM 158, UC 179"],
        },
      ]
      expect(actual).toEqual(expected)
    })
  })

  describe("The meeting time has two lectures each with one occurence", () => {
    beforeEach(() => {
      meetingTime = [
        {
          meetData: {
            cap: 69,
            code: "CSC419H1",
            enrol: 38,
            extra: 0,
            instructor: "A. Jacobson",
            section: "LEC0101",
            session: "F",
            wait: 0,
          },
          timeData: [
            {
              endHour: 17,
              firstRoom: {
                address: "80  St. George Street",
                bCode: "LM",
                bName: "Lash Miller Chemical Laboratories",
                lat: 43.66160185,
                lng: -79.39841172598216,
                postalCode: "M5S 3H6",
                room: "LM 158",
              },
              secondRoom: null,
              startHour: 15,
              weekDay: 1,
            },
          ],
        },
        {
          meetData: {
            cap: 1,
            code: "CSC419H1",
            enrol: 1,
            extra: 0,
            instructor: "A. Jacobson",
            section: "LEC2001",
            session: "F",
            wait: 0,
          },
          timeData: [
            {
              endHour: 17,
              firstRoom: {
                address: "80  St. George Street",
                bCode: "LM",
                bName: "Lash Miller Chemical Laboratories",
                lat: 43.66160185,
                lng: -79.39841172598216,
                postalCode: "M5S 3H6",
                room: "LM 158",
              },
              secondRoom: null,
              startHour: 15,
              weekDay: 1,
            },
          ],
        },
      ]
    })
    test("The lectures are sorted by the section", () => {
      actual = wrapper.getTable(meetingTime, "F")
      expected = [
        {
          activity: "LEC0101",
          instructor: "A. Jacobson",
          availability: "31 of 69 available",
          waitList: "0 students",
          time: ["Tuesday 15 - 17"],
          room: ["LM 158 "],
        },
        {
          activity: "LEC2001",
          instructor: "A. Jacobson",
          availability: "0 of 1 available",
          waitList: "0 students",
          time: ["Tuesday 15 - 17"],
          room: ["LM 158 "],
        },
      ]
      expect(actual).toEqual(expected)
    })
  })

  describe("The meeting time has a lecture and a tutorial each with one occurence", () => {
    beforeEach(() => {
      meetingTime = [
        {
          meetData: {
            cap: 166,
            code: "CSC207H1",
            enrol: 164,
            extra: 0,
            instructor: "Paul G.",
            section: "TUT0301",
            session: "F",
            wait: 0,
          },
          timeData: [
            {
              endHour: 16,
              firstRoom: null,
              secondRoom: null,
              startHour: 14,
              weekDay: 0,
            },
          ],
        },
        {
          meetData: {
            cap: 69,
            code: "CSC207H1",
            enrol: 38,
            extra: 0,
            instructor: "A. Jacobson",
            section: "LEC0101",
            session: "F",
            wait: 0,
          },
          timeData: [
            {
              endHour: 17,
              firstRoom: {
                address: "80  St. George Street",
                bCode: "LM",
                bName: "Lash Miller Chemical Laboratories",
                lat: 43.66160185,
                lng: -79.39841172598216,
                postalCode: "M5S 3H6",
                room: "LM 158",
              },
              secondRoom: null,
              startHour: 15,
              weekDay: 1,
            },
          ],
        },
      ]
    })
    test("The lecture is shown before the tutorial", () => {
      actual = wrapper.getTable(meetingTime, "F")
      expected = [
        {
          activity: "LEC0101",
          instructor: "A. Jacobson",
          availability: "31 of 69 available",
          waitList: "0 students",
          time: ["Tuesday 15 - 17"],
          room: ["LM 158 "],
        },
        {
          activity: "TUT0301",
          instructor: "Paul G.",
          availability: "2 of 166 available",
          waitList: "0 students",
          time: ["Monday 14 - 16"],
          room: ["  "],
        },
      ]
      expect(actual).toEqual(expected)
    })
  })
})
