/*
 * The actual dependencies.
 */

// Required for specialist
var reqs = [
  "CSC108", "CSC148", "CSC165", "CSC207", 
  "CSC209", "CSC236", "CSC258", "CSC263", 
  "CSC369", "CSC373", "Calc1", "Lin1", "Sta1"];

// "Inquiry" courses
var CSCinq = [
  "CSC301", "CSC318", "CSC404", "CSC411", 
  "CSC418", "CSC420", "CSC428", "CSC454", 
  "CSC485", "CSC490", "CSC491", "CSC494", "CSC495"];

// Focus-related courses
var sciFocusList = [
  "CSC336", "CSC446", "CSC456", "CSC320", 
  "CSC418", "CSC321", "CSC411", "CSC343",
   "CSC384", "CSC358", "CSC458"];
var AIFocusList = [
  "CSC310", "CSC330", "CSC438", "CSC448", 
  "CSC463", "CSC401", "CSC485", "CSC320", 
  "CSC420", "CSC321", "CSC411", "CSC412", 
  "CSC384", "CSC486"];
var NLPFocusList = [
  "CSC318", "CSC401", "CSC485", "CSC309", 
  "CSC321", "CSC330", "CSC411", "CSC428", "CSC486"];
var visionFocusList = [
  "CSC320", "CSC336", "CSC411", "CSC420", 
  "CSC418", "CSC412"];
var systemsFocusList = [
  "CSC324", "CSC343", "CSC443", "CSC469", 
  "CSC488", "CSC372", "ECE385", "CSC358", 
  "CSC458", "CSC301", "CSC309", "CSC410", "ECE489"];
var gameFocusList = [
  "CSC300", "CSC301", "CSC318", "CSC324", 
  "CSC384", "CSC418", "CSC404"];
var HCIFocusList = [
  "CSC300", "CSC301", "CSC318", "CSC428", 
  "CSC309", "CSC320", "CSC321", "CSC343", 
  "CSC384", "CSC401", "CSC404", "CSC418", 
  "CSC485", "CSC490", "CSC491"];
var theoryFocusList = [
  "CSC336", "CSC463", "CSC310", "CSC438", 
  "CSC448", "Sta2"];
var webFocusList = [
  "Sta2", "CSC309", "CSC343", "CSC358", 
  "CSC458", "CSC411", "CSC310", "CSC443", "CSC469"];

// Courses that can be taken with no other prerequisites
var initiallyTakeable = [
  "CSC104", "CSC120", "CSC108", "CSC165", 
  "Calc1", "Lin1", "CSC200", "CSC300", 
  "CSC490", "CSC491"];


// Math courses
makeNode([], "AND", "Calc1");
makeNode([Calc1], "AND", "Sta1");
makeNode([Sta1], "AND", "Sta2");
makeNode([], "AND", "Lin1");

// Math Hybrids
makeHybrid([Sta1], "AND", "hybrid6");
makeHybrid([Lin1], "AND", "hybrid7");
makeHybrid([Sta1], "AND", "hybrid8");
makeHybrid([Sta2], "AND", "hybrid9");
makeHybrid([hybrid6, hybrid7], "AND", "bool4");
makeHybrid([Calc1, Lin1], "AND", "bool5");

// First year courses
makeNode([], "AND", "CSC104");
makeNode([], "AND", "CSC108");
makeNode([], "AND", "CSC120");
makeNode([CSC108], "AND", "CSC148");
makeNode([], "AND", "CSC165");

// First year hybrids
makeHybrid([CSC148], "AND", "hybrid13")
makeHybrid([CSC165, Calc1], "OR", "hybrid16");

// Second year courses
makeNode([], "AND", "CSC200");
makeNode([CSC148], "AND", "CSC207");
makeNode([CSC207], "AND", "CSC209");
makeNode([CSC148, CSC165], "AND", "CSC236");
makeNode([CSC148, CSC165], "AND", "CSC258");

// Second year hybrids and CSC263
makeHybrid([CSC236, CSC207], "AND", "bool1");
makeHybrid([CSC209, CSC258], "AND", "bool2");
makeNode([bool1, hybrid6], "AND", "CSC263"); // CSC263
makeHybrid([CSC263], "AND", "hybrid2");
makeHybrid([CSC209], "AND", "hybrid12");
makeHybrid([CSC263], "AND", "hybrid10");
makeHybrid([CSC236], "AND", "hybrid11");
makeHybrid([CSC207], "AND", "hybrid14");
makeHybrid([CSC263], "AND", "hybrid15");

// Third year courses
makeNode([], "AND", "CSC300");
makeNode([CSC209, hybrid10], "AND", "CSC301");
makeNode([CSC301], "AND", "CSC302");
makeNode([hybrid13, bool4], "AND", "CSC310");
makeNode([], "AND", "CSC318");
makeNode([hybrid12, bool5], "AND", "CSC320");
makeNode([bool4], "AND", "CSC321");
makeNode([CSC263], "AND", "CSC324"); // CHANGED
makeNode([CSC236], "AND", "CSC330");
makeNode([CSC148, bool5], "AND", "CSC336");
makeNode([hybrid16, hybrid14], "AND", "CSC343");
makeNode([CSC209, CSC343], "AND", "CSC309");
makeNode([bool2, hybrid2], "AND", "CSC358");
makeNode([bool2], "AND", "CSC369");
makeNode([bool2], "AND", "CSC372");
makeNode([CSC263], "AND", "CSC373");
//makeNode([CSC263, hybrid8], "AND", "CSC384"); // CHANGED
makeNode([CSC263, hybrid8], "AND", "CSC384");
makeNode([bool2], "AND", "ECE385");

// Third year hybrids
makeHybrid([CSC324], "AND", "hybrid4");
makeHybrid([CSC373], "AND", "hybrid5");

// Fourth year courses
makeNode([CSC207, hybrid8], "AND", "CSC401");
makeNode([bool1], "AND", "CSC410");
makeNode([CSC263, Calc1, hybrid9], "AND", "CSC411");
makeNode([CSC411], "AND", "CSC412");
makeNode([hybrid15, bool5], "AND", "CSC420");
makeNode([CSC318, Sta2, hybrid12], "AND", "CSC428");
makeNode([CSC336], "AND", "CSC436");
makeNode([CSC343, CSC369, hybrid5], "AND", "CSC443");
makeNode([CSC336], "AND", "CSC446");
makeNode([], "AND", "CSC454");
makeNode([hybrid12, CSC336], "AND", "CSC456");
makeNode([bool2, hybrid2], "AND", "CSC458");
makeNode([CSC236], "AND", "CSC463");
makeNode([CSC463], "AND", "CSC448"); // CSC448
makeNode([CSC236], "AND", "CSC465");
makeNode([CSC369], "AND", "CSC469");
makeNode([hybrid8, CSC209], "AND", "CSC485");
makeNode([CSC258, hybrid4, hybrid2], "AND", "CSC488");
makeNode([hybrid11], "AND", "ECE489");
makeNode([], "AND", "CSC490");
makeNode([], "AND", "CSC491");
makeNode([], "AND", "CSC494");
makeNode([], "AND", "CSC495");

// Fourth year hybrids and CSC418, CSC438, and CSC486
makeHybrid([CSC373, CSC463], "OR", "bool3");
makeHybrid([CSC336, CSC373, CSC463], "OR", "CSC336orCSC373orCSC463");
makeHybrid([CSC336orCSC373orCSC463, Calc1], "AND", "hybrid3");
makeNode([hybrid3, hybrid12], "AND", "CSC418"); // CSC418
makeNode([bool3], "OR", "CSC438"); // CSC438
makeNode([CSC384, bool3], "AND", "CSC486"); // CSC486
makeHybrid([CSC318, CSC418, CSC301, CSC384], "OR", "hybrid1");
makeNode([hybrid1], "AND", "CSC404"); // CSC404


// Edges
makeEdge(CSC463, CSC448, "p1");
makeEdge(CSC165, CSC236, "p2");
makeEdge(CSC236, CSC463, "p3");
makeEdge(CSC263, CSC373, "p4");
makeEdge(CSC108, CSC148, "p5");
makeEdge(CSC148, CSC236, "p6");
makeEdge(CSC148, CSC207, "p7");
makeEdge(CSC207, CSC209, "p8");
makeEdge(CSC148, CSC258, "p9");
makeEdge(CSC165, CSC258, "p10");
makeEdge(CSC148, CSC336, "p11");
makeEdge(CSC336, CSC446, "p12");
makeEdge(CSC336, CSC456, "p13");
makeEdge(CSC336, CSC436, "p14");
makeEdge(CSC384, CSC486, "p15");
makeEdge(CSC207, CSC401, "p16");
makeEdge(CSC209, CSC485, "p17");
makeEdge(CSC263, CSC411, "p18");
makeEdge(CSC209, CSC301, "p19");
makeEdge(CSC301, CSC302, "p20");
makeEdge(CSC263, CSC384, "p21");
makeEdge(CSC369, CSC469, "p22");
makeEdge(CSC209, CSC309, "p23");
makeEdge(CSC343, CSC309, "p24");
makeEdge(CSC343, CSC443, "p25");
makeEdge(CSC369, CSC443, "p26");
makeEdge(hybrid1, CSC404, "p27");
makeEdge(hybrid2, CSC488, "p28");
makeEdge(hybrid2, CSC358, "p29");
makeEdge(hybrid2, CSC458, "p30");
makeEdge(hybrid3, CSC418, "p31");
makeEdge(CSC236, bool1, "p32");
makeEdge(CSC207, bool1, "p33");
makeEdge(bool1, CSC263, "p34");
makeEdge(bool1, CSC410, "p35");
makeEdge(hybrid4, CSC488, "p37");
makeEdge(hybrid5, CSC443, "p38");
makeEdge(CSC209, bool2, "p39");
makeEdge(CSC258, bool2, "p40");
makeEdge(bool2, CSC369, "p41");
makeEdge(bool2, ECE385, "p42");
makeEdge(bool2, CSC358, "p43");
makeEdge(bool2, CSC458, "p44");
makeEdge(CSC463, bool3, "p45");
makeEdge(CSC373, bool3, "p46");
makeEdge(bool3, CSC438, "p47");
makeEdge(bool3, CSC486, "p48");
makeEdge(Sta1, CSC263, "p49");
makeEdge(Sta1, CSC384, "p50");
makeEdge(Sta1, CSC401, "p51");
makeEdge(Sta1, CSC485, "p52");
makeEdge(Sta2, CSC411, "p54");
makeEdge(Calc1, Sta1, "p55");
makeEdge(Sta2, CSC428, "p56");
makeEdge(Sta1, Sta2, "p57");
makeEdge(CSC411, CSC412, "p58");
makeEdge(hybrid10, CSC301, "p59");
makeEdge(hybrid11, ECE489, "p60");
makeEdge(bool2, CSC372, "p61");
makeEdge(hybrid12, CSC456, "p62");
makeEdge(hybrid12, CSC428, "p63");
makeEdge(hybrid12, CSC320, "p64");
makeEdge(hybrid12, CSC418, "p65");
makeEdge(hybrid7, bool4, "p66");
makeEdge(Sta1, bool4, "p67");
makeEdge(bool4, CSC321, "p68");
makeEdge(bool4, CSC310, "p69");
makeEdge(CSC148, CSC310, "p70");
makeEdge(hybrid14, CSC343, "p71");
makeEdge(CSC263, CSC420, "p72");
makeEdge(hybrid16, CSC343, "p73");
makeEdge(Lin1, bool5, "p74");
makeEdge(Calc1, bool5, "p75");
makeEdge(bool5, CSC336, "p76");
makeEdge(bool5, CSC320, "p77");
makeEdge(bool5, CSC420, "p78");
makeEdge(CSC258, CSC488, "p79");
makeEdge(CSC318, CSC428, "p80");
makeEdge(CSC263, CSC324, "p81");