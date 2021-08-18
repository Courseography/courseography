// Focus-related courses
export var sciFocusList = [
  "csc336",
  "csc446",
  "csc456",
  "csc320",
  "csc418",
  "csc321",
  "csc411",
  "csc343",
  "csc384",
  "csc358",
  "csc458",
  "csc436",
  "csc466",
]

export var AIFocusList = [
  "csc310",
  "csc438",
  "csc448",
  "csc463",
  "csc401",
  "csc485",
  "csc320",
  "csc420",
  "csc321",
  "csc411",
  "csc412",
  "csc384",
  "csc486",
  "csc336",
  "sta248261",
]

export var NLPFocusList = [
  "csc318",
  "csc401",
  "csc485",
  "csc309",
  "csc321",
  "csc411",
  "csc428",
  "csc486",
]

export var visionFocusList = [
  "csc320",
  "csc336",
  "csc411",
  "csc420",
  "csc418",
  "csc412",
]

export var systemsFocusList = [
  "csc324",
  "csc343",
  "csc443",
  "csc469",
  "csc488",
  "csc372",
  "ece385",
  "csc358",
  "csc458",
]

export var gameFocusList = [
  "csc300",
  "csc301",
  "csc318",
  "csc324",
  "csc384",
  "csc418",
  "csc404",
]

export var HCIFocusList = [
  "csc300",
  "csc301",
  "csc318",
  "csc428",
  "csc309",
  "csc320",
  "csc321",
  "csc343",
  "csc384",
  "csc401",
  "csc404",
  "csc418",
  "csc485",
]

export var theoryFocusList = [
  "csc336",
  "csc463",
  "csc310",
  "csc438",
  "csc448",
  "sta248261",
]

export var webFocusList = [
  "sta248261",
  "csc309",
  "csc343",
  "csc358",
  "csc458",
  "csc411",
  "csc310",
  "csc443",
  "csc469",
]

// Focus descriptions
export var HCIDescription = {
  description:
    "Human-Computer Interaction (HCI) is the scientific study of the use of computers by people and" +
    " the design discipline that informs the creation of systems and software that are useful, usable," +
    " and enjoyable for the people who use them. HCI students have exciting opportunities for research" +
    " and graduate school; HCI professionals often have jobs with titles such as user interface architect," +
    " user interface specialist, interaction designer, or usability engineer. [Note 3.5 FCEs in SOC & PSY" +
    " are in addition to the 12.0 credits required to complete the Specialist program]",
  requiredCourses: [
    "CSC300H1, CSC301H1, CSC318H1, CSC428H1",
    "( SOC100H1, SOC150H1)/ SOC101Y1, SOC202H1, SOC204H1/​ SOC200H1, SOC252H1/​ SOC254H1/​ SOC302H1 [To enrol in restricted SOC" +
      " courses, please contact the CS Undergraduate Office in the July preceding the academic year in which you plan to take the course]",
    "1 FCE from the following: CSC309H1, CSC320H1, CSC321H1, CSC343H1, CSC384H1, CSC401H1, CSC404H1, CSC418H1, CSC485H1, CSC490H1/491H1",
    "PSY100H1, PSY270H1/PSY280H1",
  ],
  relatedCourses: [
    "CSC454H1, CSC290H1",
    "At least one half-course in Human Factors or Ergonomics offered by the Department of Mechanical" +
      "and Industrial Engineering, such as MIE240H, MIE343H1, MIE344H1, MIE448H1, and MIE449H1. Human factors" +
      "is a discipline closely associated with human-computer interaction that approaches problems in slightly different ways.",
    "ENV281H1, ENV381H1",
    "IRE260H1",
  ],
}

export var sciDescription = {
  description:
    "Scientific computing studies the world around us. Known and unknown quantities are related through" +
    " certain rules, e.g. physical laws, formulating mathematical problems. These problems are solved by" +
    " numerical methods implemented as algorithms and run on computers. The numerical methods are analyzed " +
    "and their performance (e.g. accuracy, efficiency) studied. Problems, such as choosing the optimal shape" +
    " for an airplane (to achieve, for example, minimal fuel consumption), finding the fair price for" +
    " derivative products of the market, or regulating the amount of radiation in medical scans, can" +
    " be modelled by mathematical expressions and solved by numerical techniques. Students wishing to" +
    " study scientific computing should have a strong background in mathematics—in particular calculus" +
    " of several variables, linear algebra, and statistics—be fluent in programming, and have a good" +
    " understanding of data structures and algorithm design.",
  requiredCourses: [
    "MAT235Y1/MAT237Y1/MAT257Y1",
    "1.5 FCE from the following: CSC336H1, CSC436H1, CSC446H1, CSC456H1, CSC466H1",
    "1 FCE from the following: CSC317H1/​ CSC320H1/​ CSC417H1/​ CSC418H1/​ CSC419H1, CSC311H1/​ CSC411H1, CSC343H1, CSC384H1, CSC358H1/​ CSC458H1",
  ],
  relatedCourses: [
    "CSC367H1",
    "MAT224H1/​ MAT240H1/​ MAT247H1, MAT334H1/​ MAT354H1, MAT337H1/​ MAT357H1",
    "It is also recommended that students in this focus consider taking a half-course or two from " +
      "the basic sciences (such as physics, chemistry, biology), as these sciences provide the sources " +
      "of many problems solved by numerical techniques.",
  ],
}

export var AIDescription = {
  description:
    "Artificial Intelligence (AI) is aimed at understanding and replicating the computational processes" +
    " underlying intelligent behaviour. These behaviours include the perception of one's environment," +
    " learning how that environment is structured, communicating with other agents, and reasoning to" +
    " guide one's actions. This focus is designed to provide students with an introduction to some of" +
    " the key scientific and technical ideas that have been developed in AI. There are four different" +
    " sub-areas of AI represented in our department: Computer Vision, Computational Linguistics, Machine" +
    " Learning, and Knowledge Representation and Reasoning. These areas cover a wide variety of ideas" +
    " and techniques. Students wanting to achieve this focus are required to take courses from at least" +
    " two of these sub-areas (as in point 2, below).",
  requiredCourses: [
    "1.0 credit from the following: CSC336H1, MAT235Y1/​ MAT237Y1/​ MAT257Y1, APM236H1, MAT224H1/​ MAT247H1, STA248H1/​ STA261H1, STA302H1, STA347H1",
    "2.5 FCEs from the following covering at least two of the four areas: (CSC401H1, CSC485H1), (CSC320H1, CSC420H1)" +
      " (CSC413H1/​ CSC421H1/​ CSC321H1, CSC311H1/​ CSC411H1, CSC412H1/​ STA414H1), (CSC304H1, CSC384H1, CSC486H1)",
    "1 FCE from the following: CSC320H1/418H1, CSC321H1/411H1, CSC343H1, CSC384H1, CSC358H1/CSC458H1",
  ],
  relatedCourses: ["CSC324H1, COG250Y1, PSY270H1, PHL232H1, PHL342H1"],
}

export var NLPDescription = {
  description:
    "How can we build and analyze systems that enable users to communicate with computers using human" +
    " language (also called natural language) and automatically process the vast amounts of data on " +
    "the web available in the form of text? The focus covers appropriate material on natural language" +
    " interfaces, as well as tools such as document summarization, intelligent search over the web, " +
    "and so on. Students considering this focus are encouraged to consider a Major in Linguistics. " +
    "[Note 0.5 credit in LIN is in addition to the 12.0 credits required to complete the Specialist program]",
  requiredCourses: [
    "CSC318H1",
    "CSC401H1, CSC485H1",
    "LIN100Y1/LIN200H1",
    "1.5 FCE from the following: CSC309H1, CSC413H1/​ CSC421H1/​ CSC321H1, CSC311H1/​ CSC411H1, CSC428H1, CSC486H1",
    "0.5 FCE from the following: PSY100H1, COG250Y1",
  ],
  relatedCourses: [
    "Other relevant CSC courses, depending on the student's interests, include other courses in " +
      "artificial intelligence such as CSC384H1 or CSC420H1. Linguistics, Psychology, and Cognitive " +
      "Science are all directly relevant to this focus, and we recommend that interested students take " +
      "additional courses from any or all of them.",
  ],
}

export var visionDescription = {
  description:
    "Computer vision is the science and technology of machines that can see. As a science, the goal" +
    " of computer vision is to understand the computational processes required for a machine to come" +
    " to an understanding of the content of a set of images. The data here may be a single snapshot, " +
    "a video sequence, or a set of images from different viewpoints or provided by medical scanners." +
    " The computer vision focus introduces students to the study of vision from a computational point" +
    " of view. That is, we attempt to clearly define computational problems for various steps of the" +
    " overall process, and then show how these problems can be tackled with appropriate algorithms." +
    " Students who wish to pursue computer vision should have an understanding of linear algebra and" +
    " calculus of several variables. Moreover, they should be solid programmers and have a good understanding" +
    " of data structures and algorithm design. These basic tools are required in order to first pose" +
    " computational vision problems, and then develop and test algorithms for the solution to those problems.",
  requiredCourses: [
    "MAT235Y1/​ MAT237Y1/​ MAT257Y1, CSC320H1, CSC336H1, CSC311H1/​ CSC411H1, CSC420H1",
    "0.5 credit from the following: CSC412H1, CSC417H1, CSC317H1/​ CSC418H1, CSC419H1, CSC2503H (Note: students must petition to take a graduate course.)",
  ],
  relatedCourses: [
    "The following are examples of topics and courses that fit naturally with a study of computational" +
      " vision. The list is meant to be illustrative of the range of cognate topics, but is not necessarily" +
      " complete. The ordering is alphabetical and not indicative of importance. Note: there are prerequisites" +
      " for many of these courses that we do not list here.",
    "APM462H1, COG250Y1, CSC384H1, CSC485H1, CSC486H1, ECE216H1, PHL232H1, PHY385H1, PSL440Y1, PSY270H1, PSY280H1, STA257H1/​ STA261H1",
  ],
}

export var systemsDescription = {
  description:
    "Software systems are complex and interesting. Poorly done systems can be incredibly expensive: " +
    "they can cost society billions of dollars and sometimes make the difference between life and death." +
    " Rapid changes in technology and applications means that the underlying systems must continually adapt." +
    " This focus takes you under the covers of software systems, laying bare the layers and introducing you" +
    " to concurrency issues, scalability, multiprocessor systems, distributed computing, and more.",
  requiredCourses: [
    "CSC343H1, CSC367H1, CSC469H1",
    "1.0 credit from the following: CSC358H1, CSC443H1, CSC458H1",
    "1.0 credit from the following: CSC358H1/​ CSC458H1 (if not taken in list 2), CSC324H1, CSC385H1, CSC488H1",
  ],
  relatedCourses: [
    "CSC301H1, CSC309H1, CSC410H1",
    "Relevant courses offered at UTM: CSC347H5, CSC423H5, CSC427H5",
    "Relevant courses offered by Engineering: ECE454H1, ECE568H1",
  ],
}

export var gameDescription = {
  description:
    "Video game design combines several disciplines within computer science, including software engineering," +
    " graphics, artificial intelligence, and human-computer interaction. It also incorporates elements" +
    " of economics, psychology, music, and creative writing, requiring video game researchers to have" +
    " a diverse, multidisciplinary set of skills. Students who wish to pursue video game design should" +
    " have an understanding of linear algebra (for computer graphics modelling), computer hardware and " +
    "operating systems (for console architecture), data structures, and algorithm design. Students will" +
    " gain a general knowledge of the more advanced topics listed in the courses below.",
  requiredCourses: [
    "CSC300H1, CSC301H1, CSC318H1, CSC384H1, CSC317H1/​ CSC417H1/​ CSC418H1/​ CSC419H1, CSC404H1",
  ],
  relatedCourses: [
    "CSC303H1, CSC304H1, CSC358H1, CSC458H1, CSC428H1",
    "MUS300H1, CIN212H1/​ INI222H1, CIN432H1/​ INI465H1, ENG235H1",
    "ECO326H1, RSM482H1/​MGT2056H",
  ],
}

export var theoryDescription = {
  description:
    "Why is it easy to sort a list of numbers, but hard to break Internet encryption schemes? Is finding" +
    " a solution to a problem harder than checking that a solution is correct? Can we find good approximate" +
    " solutions, even when the exact solutions seem out of reach? Theory of Computation studies the inherent" +
    " complexity of fundamental algorithmic problems. On one hand, we develop ground-breaking efficient data" +
    " structures and algorithms. On the other, we have yet to develop good algorithms for many problems despite" +
    " decades of effort, and for these problems we strive to prove no time- or space-efficient algorithms will" +
    " ever solve them. While the field has seen some successful impossibility results, there are still many " +
    "problems (such as those underlying modern cryptography and security) for which we do not know either" +
    " efficient algorithms or strong lower bounds! This focus takes a rigorous, mathematical approach" +
    " to computational problem-solving: students will gain a deep understanding of algorithm paradigms" +
    " and measures of problem complexity, and develop the skills necessary to convey abstract ideas with " +
    "precision and clarity. Many of our students go on to graduate studies and sophisticated algorithmic" +
    " work in industry. This focus has natural ties with many branches of mathematics and is the foundation" +
    " of many computer science fields. Consequently, our students often apply their theoretical knowledge" +
    " to other fields of interest. We strongly encourage taking the enriched theory courses ( CSC240H1, CSC265H1)" +
    " as well as specialist/major versions of the MAT requirements for our focus. [Depending on courses selected" +
    " for points 3 & 4, students may need to complete 0.5–1.0 credit in addition to the 12.0 credits required to complete the Specialist program.]",
  requiredCourses: [
    "MAT137Y1/​ MAT157Y1/​ MAT237Y1 (Note: If MAT237Y1 is used here, it cannot be counted as part of the 2.0 credits for point 4, below.)",
    "CSC463H1",
    "2.0 credits from the following: CSC304H1, CSC336H1, CSC438H1, CSC448H1, CSC473H1; MAT309H1, MAT332H1, MAT344H1; at UTM: MAT302H5; graduate courses: CSC2221H1, CSC2401H1, CSC2410H1, CSC2412H1, CSC2420H1, CSC2421H1, CSC2426H1, CSC2451H1, CSC2556H1 (note that students must petition to take a graduate course)",
    "2.0 credits from the following: APM236H1/​MIE262H1, MIE263H1, APM421H1, APM461H1, MAT224H1/​ MAT247H1, MAT237Y1/​ MAT257Y1, MAT244H1/​ MAT267H1, MAT301H1/​ MAT347Y1, MAT315H1, MAT327H1, MAT334H1/​ MAT354H1, MAT335H1, MAT337H1/​ MAT357H1, any 400-level MAT course, STA248H1/​ STA261H1, STA347H1",
    "Note: Students who complete an independent study project ( CSC494H1/​ CSC495H1) under the supervision of a faculty member from the Theory group may request to substitute one of CSC494H1/​ CSC495H1 for one of the courses in list 3 above. This request must be made directly to the department's Undergraduate Office.",
    "Note: Students who complete a graduate Topics course in Theory may request to count it towards the completion of list 3 above. This request must be made directly to the department's Undergraduate Office.",
  ],
  relatedCourses: [
    "Students are strongly encouraged to take the enriched theory courses: CSC240H1 and CSC265H1, rather than their regular counterparts: CSC165H1/​ CSC236H1 and CSC263H1, respectively.",
  ],
}

export var webDescription = {
  description:
    "The Web and Internet Technologies focus introduces students to the systems and algorithms that " +
    "power today's large-scale web and Internet applications such as search engines, social networking" +
    " applications, web data mining applications, and content distribution networks. The focus covers the" +
    " algorithm foundations of web and internet technologies, as well as implementation and system architecture." +
    " Students who wish to pursue the Focus in Web and Internet Technologies should have a solid understanding" +
    " of statistics, be good programmers, and have a good understanding of data structures and algorithm design." +
    " To get practical experience, students pursuing the web and Internet technologies focus are encouraged to do either a term project or a summer USRA project in web and internet technologies.",
  requiredCourses: [
    "STA248H1/​ STA261H1, CSC309H1, CSC343H1, CSC358H1/​ CSC457H1, CSC458H1, CSC311H1/​ CSC411H1",
    "0.5 credit from the following: CSC367H1, CSC443H1, CSC469H1",
  ],
  relatedCourses: [
    "Courses offered at UTM: CSC347H5, CSC423H5, CSC427H5",
    "ECE568H1",
    "ENV281H1, ENV381H1",
  ],
}

export const computerScienceFocusData = {
  web: ["Web Technologies", webDescription],
  theory: ["Theory of Computation", theoryDescription],
  HCI: ["Human\u2011Computer Interaction", HCIDescription],
  game: ["Game Design", gameDescription],
  systems: ["Computer Systems", systemsDescription],
  vision: ["Computer Vision", visionDescription],
  NLP: ["Computational Linguistics", NLPDescription],
  AI: ["Artificial Intelligence", AIDescription],
  sci: ["Scientific Computing", sciDescription],
}
