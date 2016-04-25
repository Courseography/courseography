{-# LANGUAGE OverloadedStrings #-}

module Css.Constants
    (margin0,
     padding0,
     width100,
     height100,
     fill,
     stroke,
     alignCenter,
     wideStroke,
     faded,
     semiVisible,
     fullyVisible,
     strokeRed,
     strokeDashed,
     roundCorners,
     theoryDark,
     coreDark,
     seDark,
     systemsDark,
     graphicsDark,
     dbwebDark,
     numDark,
     aiDark,
     hciDark,
     mathDark,
     introDark,
     titleColour,
     lightGrey,
     purple1,
     purple2,
     purple3,
     purple4,
     purple5,
     purple6,
     purple7,
     purple8,
     purple9,
     purple10,
     pink1,
     pink2,
     borderNone,
     borderPink,
     teal1,
     orange1,
     blue1,
     blue2,
     blue3,
     blue4,
     blue5,
     blue6,
     blueFb,
     red1,
     red2,
     red3,
     red4,
     red5,
     green1,
     dRed,
     dGreen,
     dBlue,
     dPurple,
     grey1,
     grey2,
     grey3,
     grey4,
     grey5,
     beige1,
     fceCountColor,
     nodeFontSize,
     hybridFontSize,
     boolFontSize,
     regionFontSize,
     absoluteZero,
     acajou,
     acidGreen,
     aero,
     aeroBlue,
     africanViolet,
     airForceBlue,
     airSuperiorityBlue,
     alabamaCrimson,
     alienArmpit,
     alizarinCrimson,
     alloyOrange,
     amaranth,
     amaranthDeepPurple,
     amaranthPink,
     amaranthPurple,
     amaranthRed,
     amazon,
     amber,
     amberSAE,
     americanRose,
     amethyst,
     androidGreen,
     antiFlashWhite,
     antiqueBrass,
     antiqueBronze,
     antiqueFuchsia,
     antiqueRuby,
     ao,
     appleGreen,
     apricot,
     arcticLime,
     armyGreen,
     arsenic,
     artichoke,
     arylideYellow,
     ashGrey,
     asparagus,
     atomicTangerine,
     auburn,
     aureolin,
     auroMetalSaurus,
     avocado,
     aztecGold,
     azureishWhite,
     azureMist,
     azureWebColor,
     babyBlue,
     babyBlueEyes,
     babyPink,
     babyPowder,
     bakerMillerPink,
     ballBlue,
     bananaMania,
     bananaYellow,
     bangladeshGreen,
     barbiePink,
     barnRed,
     batteryChargedBlue,
     battleshipGrey,
     bazaar,
     bDazzledBlue,
     beauBlue,
     beaver,
     begonia,
     bigDipORuby,
     bigFootFeet,
     bistre,
     bistreBrown,
     bitterLemon,
     bitterLime,
     bittersweet,
     bittersweetShimmer,
     blackBean,
     blackCoral,
     blackLeatherJacket,
     blackOlive,
     blackShadows,
     blastOffBronze,
     blazeOrange,
     bleuDeFrance,
     blizzardBlue,
     blond,
     blueBell,
     blueberry,
     blueBolt,
     bluebonnet,
     blueCrayola,
     blueGray,
     blueGreen,
     blueJeans,
     blueLagoon,
     blueMagentaViolet,
     blueMunsell,
     blueNCS,
     bluePantone,
     bluePigment,
     blueRYB,
     blueSapphire,
     blueYonder,
     bole,
     bondiBlue,
     bone,
     boogerBuster,
     bostonUniversityRed,
     bottleGreen,
     boysenberry,
     brandeisBlue,
     brass,
     brickRed,
     brightCerulean,
     brightGreen,
     brightLavender,
     brightLilac,
     brightMaroon,
     brightNavyBlue,
     brightPink,
     brightTurquoise,
     brightUbe,
     brightYellowCrayola,
     brilliantAzure,
     brilliantLavender,
     brilliantRose,
     brinkPink,
     britishRacingGreen,
     bronze,
     bronzeYellow,
     brownNose,
     brownSugar,
     brownTraditional,
     brownWeb,
     brownYellow,
     brunswickGreen,
     bubbleGum,
     bubbles,
     budGreen,
     buff,
     bulgarianRose,
     burgundy,
     burnishedBrown,
     burntOrange,
     burntSienna,
     burntUmber,
     byzantine,
     byzantium,
     cadetGrey,
     cadmiumGreen,
     cadmiumOrange,
     cadmiumRed,
     cadmiumYellow,
     cafeAuLait,
     cafeNoir,
     calPolyGreen,
     cambridgeBlue,
     camel,
     cameoPink,
     camouflageGreen,
     canary,
     canaryYellow,
     candyAppleRed,
     candyPink,
     capri,
     caputMortuum,
     cardinal,
     caribbeanGreen,
     carmine,
     carmineMP,
     carminePink,
     carmineRed,
     carnationPink,
     carnelian,
     carolinaBlue,
     carrotOrange,
     castletonGreen,
     catalinaBlue,
     catawba,
     cedarChest,
     ceil,
     celadon,
     celadonBlue,
     celadonGreen,
     celeste,
     celestialBlue,
     cerise,
     cerisePink,
     cerulean,
     ceruleanBlue,
     ceruleanFrost,
     cGBlue,
     cGRed,
     chamoisee,
     champagne,
     champagnePink,
     charcoal,
     charlestonGreen,
     charmPink,
     chartreuseTraditional,
     chartreuseWeb,
     cherry,
     cherryBlossomPink,
     chestnut,
     chinaPink,
     chinaRose,
     chineseRed,
     chineseViolet,
     chlorophyllGreen,
     chocolateTraditional,
     chocolateWeb,
     chromeYellow,
     cinereous,
     cinnabar,
     cinnamon,
     cinnamonSatin,
     citrine,
     citron,
     claret,
     classicRose,
     cobaltBlue,
     cocoaBrown,
     coconut,
     coffee,
     columbiaBlue,
     congoPink,
     coolBlack,
     coolGrey,
     copper,
     copperCrayola,
     copperPenny,
     copperRed,
     copperRose,
     coquelicot,
     coralPink,
     coralRed,
     coralReef,
     cordovan,
     cornellRed,
     cosmicCobalt,
     cosmicLatte,
     cottonCandy,
     coyoteBrown,
     crimsonGlory,
     crimsonRed,
     cultured,
     cyanAzure,
     cyanBlueAzure,
     cyanCobaltBlue,
     cyanCornflowerBlue,
     cyanProcess,
     cyberGrape,
     cyberYellow,
     cyclamen,
     daffodil,
     dandelion,
     darkBlueGray,
     darkBrown,
     darkBrownTangelo,
     darkByzantium,
     darkCandyAppleRed,
     darkCerulean,
     darkChestnut,
     darkCoral,
     darkElectricBlue,
     darkGunmetal,
     darkImperialBlue,
     darkImperialBlue2,
     darkJungleGreen,
     darkLava,
     darkLavender,
     darkLiver,
     darkLiverHorses,
     darkMediumGray,
     darkMidnightBlue,
     darkMossGreen,
     darkPastelBlue,
     darkPastelGreen,
     darkPastelPurple,
     darkPastelRed,
     darkPink,
     darkPowderBlue,
     darkPuce,
     darkPurple,
     darkRaspberry,
     darkScarlet,
     darkSienna,
     darkSkyBlue,
     darkSpringGreen,
     darkTan,
     darkTangerine,
     darkTaupe,
     darkTerraCotta,
     darkVanilla,
     darkYellow,
     dartmouthGreen,
     davysGrey,
     debianRed,
     deepAquamarine,
     deepCarmine,
     deepCarminePink,
     deepCarrotOrange,
     deepCerise,
     deepChampagne,
     deepChestnut,
     deepCoffee,
     deepFuchsia,
     deepGreen,
     deepGreenCyanTurquoise,
     deepJungleGreen,
     deepKoamaru,
     deepLemon,
     deepLilac,
     deepMagenta,
     deepMaroon,
     deepMauve,
     deepMossGreen,
     deepPeach,
     deepPuce,
     deepRed,
     deepRuby,
     deepSaffron,
     deepSpaceSparkle,
     deepSpringBud,
     deepTaupe,
     deepTuscanRed,
     deepViolet,
     deer,
     denim,
     denimBlue,
     desaturatedCyan,
     desert,
     desertSand,
     desire,
     diamond,
     dingyDungeon,
     dirt,
     dogwoodRose,
     dollarBill,
     dolphinGray,
     donkeyBrown,
     dukeBlue,
     dustStorm,
     dutchWhite,
     earthYellow,
     ebony,
     ecru,
     eerieBlack,
     eggplant,
     eggshell,
     egyptianBlue,
     electricBlue,
     electricCrimson,
     electricCyan,
     electricGreen,
     electricIndigo,
     electricLavender,
     electricLime,
     electricPurple,
     electricUltramarine,
     electricViolet,
     electricYellow,
     emerald,
     eminence,
     englishGreen,
     englishLavender,
     englishRed,
     englishVermillion,
     englishViolet,
     etonBlue,
     eucalyptus,
     fallow,
     faluRed,
     fandango,
     fandangoPink,
     fashionFuchsia,
     fawn,
     feldgrau,
     feldspar,
     fernGreen,
     ferrariRed,
     fieldDrab,
     fieryRose,
     fireEngineRed,
     flame,
     flamingoPink,
     flattery,
     flavescent,
     flax,
     flirt,
     fluorescentOrange,
     fluorescentPink,
     fluorescentYellow,
     folly,
     forestGreenTraditional,
     forestGreenWeb,
     frenchBeige,
     frenchBistre,
     frenchBlue,
     frenchFuchsia,
     frenchLilac,
     frenchLime,
     frenchMauve,
     frenchPink,
     frenchPlum,
     frenchPuce,
     frenchRaspberry,
     frenchRose,
     frenchSkyBlue,
     frenchViolet,
     frenchWine,
     freshAir,
     frostbite,
     fuchsiaCrayola,
     fuchsiaPink,
     fuchsiaPurple,
     fuchsiaRose,
     fulvous,
     fuzzyWuzzy,
     gamboge,
     gambogeOrangeBrown,
     gargoyleGas,
     genericViridian,
     giantsClub,
     giantsOrange,
     ginger,
     glaucous,
     glitter,
     glossyGrape,
     gOGreen,
     goldenBrown,
     goldenPoppy,
     goldenYellow,
     goldFusion,
     goldMetallic,
     goldWeb,
     graniteGray,
     grannySmithApple,
     grape,
     grayAsparagus,
     grayBlue,
     grayX11,
     greenBlue,
     greenCrayola,
     greenCyan,
     greenHTML,
     greenLizard,
     greenMunsell,
     greenNCS,
     greenPantone,
     greenPigment,
     greenRYB,
     greenSheen,
     greenX11,
     grizzly,
     grullo,
     gunmetal,
     guppieGreen,
     halayaUbe,
     hanBlue,
     hanPurple,
     hansaYellow,
     harlequin,
     harlequinGreen,
     harvardCrimson,
     harvestGold,
     heartGold,
     heatWave,
     heidelbergRed,
     heliotrope,
     heliotropeGray,
     heliotropeMagenta,
     hollywoodCerise,
     honoluluBlue,
     hookersGreen,
     hotMagenta,
     hunterGreen,
     iceberg,
     icterine,
     iguanaGreen,
     illuminatingEmerald,
     imperial,
     imperialBlue,
     imperialPurple,
     imperialRed,
     inchworm,
     independence,
     indiaGreen,
     indianYellow,
     indigoDye,
     indigoWeb,
     infraRed,
     interdimensionalBlue,
     internationalKleinBlue,
     internationalOrangeAerospace,
     internationalOrangeEngineering,
     internationalOrangeGoldenGateBridge,
     iris,
     irresistible,
     isabelline,
     islamicGreen,
     italianSkyBlue,
     jade,
     japaneseCarmine,
     japaneseIndigo,
     japaneseViolet,
     jasmine,
     jasper,
     jazzberryJam,
     jellyBean,
     jet,
     jonquil,
     jordyBlue,
     juneBud,
     jungleGreen,
     kellyGreen,
     kenyanCopper,
     keppel,
     keyLime,
     khakiHTML,
     kiwi,
     kobe,
     kobi,
     kobicha,
     kombuGreen,
     kSUPurple,
     kUCrimson,
     languidLavender,
     lapisLazuli,
     laSalleGreen,
     laserLemon,
     laurelGreen,
     lava,
     lavenderBlue,
     lavenderFloral,
     lavenderGray,
     lavenderIndigo,
     lavenderMagenta,
     lavenderMist,
     lavenderPink,
     lavenderPurple,
     lavenderRose,
     lavenderWeb,
     lemonCurry,
     lemonGlacier,
     lemonLime,
     lemonMeringue,
     lemonYellow,
     liberty,
     licorice,
     lightApricot,
     lightBrown,
     lightCarminePink,
     lightCobaltBlue,
     lightCornflowerBlue,
     lightCrimson,
     lightDeepPink,
     lightFrenchBeige,
     lightFuchsiaPink,
     lightGrayishMagenta,
     lightHotPink,
     lightKhaki,
     lightMediumOrchid,
     lightMossGreen,
     lightOrchid,
     lightPastelPurple,
     lightRedOchre,
     lightSalmonPink,
     lightTaupe,
     lightThulianPink,
     lilac,
     lilacLuster,
     limerick,
     limeWeb,
     lincolnGreen,
     lion,
     liseranPurple,
     littleBoyBlue,
     liver,
     liverChestnut,
     liverDogs,
     liverOrgan,
     livid,
     lumber,
     lust,
     maastrichtBlue,
     macaroniAndCheese,
     madderLake,
     magentaCrayola,
     magentaDye,
     magentaHaze,
     magentaPantone,
     magentaPink,
     magentaProcess,
     magicMint,
     magicPotion,
     magnolia,
     mahogany,
     maize,
     majorelleBlue,
     malachite,
     manatee,
     mandarin,
     mangoTango,
     mantis,
     mardiGras,
     marigold,
     maroonCrayola,
     maroonHTML,
     maroonX11,
     mauve,
     mauvelous,
     mauveTaupe,
     maximumBlue,
     maximumBlueGreen,
     maximumBluePurple,
     maximumGreen,
     maximumGreenYellow,
     maximumPurple,
     maximumRed,
     maximumRedPurple,
     maximumYellow,
     maximumYellowRed,
     mayaBlue,
     mayGreen,
     meatBrown,
     mediumCandyAppleRed,
     mediumCarmine,
     mediumChampagne,
     mediumElectricBlue,
     mediumJungleGreen,
     mediumLavenderMagenta,
     mediumPersianBlue,
     mediumRedViolet,
     mediumRuby,
     mediumSkyBlue,
     mediumSpringBud,
     mediumTaupe,
     mediumTuscanRed,
     mediumVermilion,
     mellowApricot,
     mellowYellow,
     melon,
     metallicSeaweed,
     metallicSunburst,
     metalPink,
     mexicanPink,
     middleBlue,
     middleBlueGreen,
     middleBluePurple,
     middleGreen,
     middleGreenYellow,
     middlePurple,
     middleRed,
     middleRedPurple,
     middleRedPurple2,
     middleYellow,
     middleYellowRed,
     midnightGreenEagleGreen,
     mikadoYellow,
     mimiPink,
     mindaro,
     ming,
     minionYellow,
     mintGreen,
     mistyMoss,
     modeBeige,
     moonstoneBlue,
     mordantRed19,
     mossGreen,
     mountainMeadow,
     mountbattenPink,
     mSUGreen,
     mughalGreen,
     mulberry,
     mummysTomb,
     mustard,
     myrtleGreen,
     mystic,
     mysticMaroon,
     nadeshikoPink,
     napierGreen,
     naplesYellow,
     navyPurple,
     neonCarrot,
     neonFuchsia,
     neonGreen,
     newCar,
     newYorkPink,
     nickel,
     nonPhotoBlue,
     northTexasGreen,
     nyanza,
     oceanBlue,
     oceanBoatBlue,
     oceanGreen,
     ochre,
     officeGreen,
     ogreOdor,
     oldBurgundy,
     oldGold,
     oldHeliotrope,
     oldLavender,
     oldMauve,
     oldMossGreen,
     oldRose,
     oldSilver,
     oliveDrab3,
     oliveDrab7,
     olivine,
     onyx,
     operaMauve,
     orangeColorWheel,
     orangeCrayola,
     orangePantone,
     orangePeel,
     orangeRYB,
     orangeSoda,
     orangeWeb,
     orangeYellow,
     orchidPink,
     oriolesOrange,
     otterBrown,
     oUCrimsonRed,
     outerSpace,
     outrageousOrange,
     oxfordBlue,
     pacificBlue,
     pakistanGreen,
     palatinateBlue,
     palatinatePurple,
     paleAqua,
     paleBlue,
     paleBrown,
     paleCarmine,
     paleCerulean,
     paleChestnut,
     paleCopper,
     paleCornflowerBlue,
     paleCyan,
     paleLavender,
     paleMagenta,
     paleMagentaPink,
     palePink,
     palePlum,
     paleRedViolet,
     paleRobinEggBlue,
     paleSilver,
     paleSpringBud,
     paleTaupe,
     palmLeaf,
     pansyPurple,
     paoloVeroneseGreen,
     paradisePink,
     parisGreen,
     parrotPink,
     pastelBlue,
     pastelBrown,
     pastelGray,
     pastelGreen,
     pastelMagenta,
     pastelOrange,
     pastelPink,
     pastelPurple,
     pastelRed,
     pastelViolet,
     pastelYellow,
     patriarch,
     paynesGrey,
     peachOrange,
     peachYellow,
     pear,
     pearl,
     pearlAqua,
     pearlyPurple,
     peridot,
     periwinkle,
     permanentGeraniumLake,
     persianBlue,
     persianGreen,
     persianIndigo,
     persianOrange,
     persianPink,
     persianPlum,
     persianRed,
     persianRose,
     persimmon,
     pewterBlue,
     phlox,
     phthaloBlue,
     phthaloGreen,
     pictonBlue,
     pictorialCarmine,
     piggyPink,
     pineapple,
     pineGreen,
     pinkFlamingo,
     pinkLace,
     pinkLavender,
     pinkOrange,
     pinkPantone,
     pinkPearl,
     pinkRaspberry,
     pinkSherbet,
     pistachio,
     pixiePowder,
     platinum,
     plumpPurple,
     plumWeb,
     polishedPine,
     pompAndPower,
     popstar,
     portlandOrange,
     princessPerfume,
     princetonOrange,
     prune,
     prussianBlue,
     psychedelicPurple,
     puce,
     puceRed,
     pullmanBrown,
     pullmanGreen,
     pumpkin,
     purpleHeart,
     purpleHTML,
     purpleMountainMajesty,
     purpleMunsell,
     purpleNavy,
     purplePizzazz,
     purplePlum,
     purpleTaupe,
     purpleX11,
     purpureus,
     quartz,
     queenBlue,
     queenPink,
     quickSilver,
     quinacridoneMagenta,
     rackley,
     radicalRed,
     raisinBlack,
     rajah,
     raspberry,
     raspberryGlace,
     raspberryPink,
     raspberryRose,
     rawSienna,
     rawUmber,
     razzleDazzleRose,
     razzmatazz,
     razzmicBerry,
     rebeccaPurple,
     redBrown,
     redCrayola,
     redDevil,
     redMunsell,
     redNCS,
     redOrange,
     redPantone,
     redPigment,
     redPurple,
     redRYB,
     redSalsa,
     redViolet,
     redwood,
     regalia,
     resolutionBlue,
     rhythm,
     richBlack,
     richBlackFOGRA29,
     richBlackFOGRA39,
     richBrilliantLavender,
     richCarmine,
     richElectricBlue,
     richLavender,
     richLilac,
     richMaroon,
     rifleGreen,
     roastCoffee,
     robinEggBlue,
     rocketMetallic,
     romanSilver,
     roseBonbon,
     roseDust,
     roseEbony,
     roseGold,
     roseMadder,
     rosePink,
     roseQuartz,
     roseRed,
     roseTaupe,
     roseVale,
     rosewood,
     rossoCorsa,
     royalAzure,
     royalFuchsia,
     royalPurple,
     royalYellow,
     ruber,
     rubineRed,
     rubyRed,
     ruddy,
     ruddyBrown,
     ruddyPink,
     rufous,
     russet,
     russianGreen,
     russianViolet,
     rust,
     rustyRed,
     sacramentoStateGreen,
     safetyOrange,
     safetyYellow,
     saffron,
     sage,
     salmonPink,
     sandDune,
     sandstorm,
     sandyTan,
     sandyTaupe,
     sangria,
     sapGreen,
     sapphire,
     sapphireBlue,
     sasquatchSocks,
     satinSheenGold,
     scarlet,
     scarlet2,
     schaussPink,
     schoolBusYellow,
     screaminGreen,
     seaBlue,
     seaFoamGreen,
     sealBrown,
     seaSerpent,
     selectiveYellow,
     shadow,
     shadowBlue,
     shampoo,
     shamrockGreen,
     sheenGreen,
     shimmeringBlush,
     shinyShamrock,
     shockingPink,
     shockingPinkCrayola,
     silverChalice,
     silverLakeBlue,
     silverPink,
     silverSand,
     sinopia,
     sizzlingRed,
     sizzlingSunrise,
     skobeloff,
     skyMagenta,
     slimyGreen,
     smaltDarkPowderBlue,
     smashedPumpkin,
     smitten,
     smokeyTopaz,
     smokyBlack,
     smokyTopaz,
     soap,
     solidPink,
     sonicSilver,
     spaceCadet,
     spanishBistre,
     spanishBlue,
     spanishCarmine,
     spanishCrimson,
     spanishGray,
     spanishGreen,
     spanishOrange,
     spanishPink,
     spanishRed,
     spanishSkyBlue,
     spanishViolet,
     spanishViridian,
     spartanCrimson,
     spicyMix,
     spiroDiscoBall,
     springBud,
     springFrost,
     starCommandBlue,
     steelPink,
     steelTeal,
     stilDeGrainYellow,
     stizza,
     stormcloud,
     stPatricksBlue,
     straw,
     strawberry,
     sugarPlum,
     sunburntCyclops,
     sunglow,
     sunny,
     sunray,
     sunset,
     sunsetOrange,
     superPink,
     sweetBrown,
     tangelo,
     tangerine,
     tangerineYellow,
     tangoPink,
     tartOrange,
     taupe,
     taupeGray,
     teaGreen,
     tealBlue,
     tealDeer,
     tealGreen,
     teaRose,
     teaRose2,
     telemagenta,
     tenne,
     terraCotta,
     thulianPink,
     tickleMePink,
     tiffanyBlue,
     tigersEye,
     timberwolf,
     titaniumYellow,
     toolbox,
     topaz,
     tractorRed,
     trolleyGrey,
     tropicalRainForest,
     tropicalViolet,
     trueBlue,
     tuftsBlue,
     tulip,
     tumbleweed,
     turkishRose,
     turquoiseBlue,
     turquoiseGreen,
     turquoiseSurf,
     turtleGreen,
     tuscan,
     tuscanBrown,
     tuscanRed,
     tuscanTan,
     tuscany,
     twilightLavender,
     tyrianPurple,
     uABlue,
     uARed,
     ube,
     uCLABlue,
     uCLAGold,
     uFOGreen,
     ultramarine,
     ultramarineBlue,
     ultraPink,
     ultraRed,
     umber,
     unbleachedSilk,
     unitedNationsBlue,
     universityOfCaliforniaGold,
     universityOfTennesseeOrange,
     unmellowYellow,
     uPForestGreen,
     uPMaroon,
     upsdellRed,
     urobilin,
     uSAFABlue,
     uSCCardinal,
     uSCGold,
     utahCrimson,
     vanDykeBrown,
     vanilla,
     vanillaIce,
     vegasGold,
     venetianRed,
     verdigris,
     vermilion,
     vermilion2,
     veronica,
     veryLightAzure,
     veryLightBlue,
     veryLightMalachiteGreen,
     veryLightTangelo,
     veryPaleOrange,
     veryPaleYellow,
     vESPIERose,
     violetBlue,
     violetColorWheel,
     violetRYB,
     violetWeb,
     viridian,
     viridianGreen,
     vistaBlue,
     vividAmber,
     vividAuburn,
     vividBurgundy,
     vividCerise,
     vividCerulean,
     vividCrimson,
     vividGamboge,
     vividLimeGreen,
     vividMalachite,
     vividMulberry,
     vividOrange,
     vividOrangePeel,
     vividOrchid,
     vividRaspberry,
     vividRed,
     vividRedTangelo,
     vividSkyBlue,
     vividTangelo,
     vividTangerine,
     vividVermilion,
     vividViolet,
     vividYellow,
     volt,
     wageningenGreen,
     warmBlack,
     waterspout,
     weldonBlue,
     wenge,
     wildBlueYonder,
     wildOrchid,
     wildStrawberry,
     wildWatermelon,
     willpowerOrange,
     windsorTan,
     wine,
     wineDregs,
     wintergreenDream,
     winterSky,
     winterWizard,
     wisteria,
     woodBrown,
     xanadu,
     yaleBlue,
     yankeesBlue,
     yellowCrayola,
     yellowMunsell,
     yellowNCS,
     yellowOrange,
     yellowPantone,
     yellowProcess,
     yellowRose,
     yellowRYB,
     yellowSunshine,
     zaffre,
     zinnwalditeBrown,
     zomp
     ) where

import Clay
import Prelude hiding ((**), tan)
import Data.Text as T


{- Empty padding and margins. -}
margin0 :: Css
margin0 = margin nil nil nil nil
padding0 :: Css
padding0 = padding nil nil nil nil

{- Default rectangle height and width,
 - set to 100 percent. -}
width100 :: Css
width100 = width $ pct 100
height100 :: Css
height100 = height $ pct 100

{- Node and rectangle constants,
 - including sizes, strokes, fills,
 - opacities, colors and alignments. -}

-- Stroke & fill
fill :: Text -> Css
fill = (-:) "fill"
stroke :: Text -> Css
stroke = (-:) "stroke"

-- Center alignment
alignCenter :: Css
alignCenter = textAlign $ alignSide sideCenter
wideStroke :: Css
wideStroke = "stroke-width" -: "3"
faded :: Css
faded = opacity 0.4
semiVisible :: Css
semiVisible = opacity 0.7
fullyVisible :: Css
fullyVisible = opacity 1.0

strokeRed :: Css
strokeRed = do
    "stroke" -: "#CC0011"
    "stroke-width" -: "2px"
strokeDashed :: Css
strokeDashed = do
    "stroke-dasharray" -: "8,5"
    "stroke-width" -: "2px"

roundCorners :: Css
roundCorners = "border-radius" -: "8px"

-- Colors

theoryDark :: T.Text
theoryDark = "#B1C8D1"

coreDark :: T.Text
coreDark = "#C9C9C9"

seDark :: T.Text
seDark = "#E68080"

systemsDark :: T.Text
systemsDark = "#C285FF"

graphicsDark :: T.Text
graphicsDark = "#66A366"

dbwebDark :: T.Text
dbwebDark = "#C42B97"

numDark :: T.Text
numDark = "#B8FF70"

aiDark :: T.Text
aiDark = "#80B2FF"

hciDark :: T.Text
hciDark = "#91F27A"

mathDark :: T.Text
mathDark = "#8A67BE"

introDark :: T.Text
introDark = "#5DD5B8"

titleColour :: T.Text
titleColour = "#072D68"

lightGrey :: T.Text
lightGrey = "#CCCCCC"

{- Background colors. -}
purple1 :: Color
purple1 = parse "#46364A"
purple2 :: Color
purple2 = parse "#7E4D66"
purple3 :: Color
purple3 = parse "#CD96CD"
purple4 :: Color
purple4 = parse "#9C6B98"
purple5 :: Color
purple5 = parse "#800080"

purple6 :: Color
purple6 = parse "#CAC4D4"
purple7 :: Color
purple7 = parse "#9C91B0"
purple8 :: Color
purple8 = parse "#7A6A96"
purple9 :: Color
purple9 = parse "#433063"
purple10 :: Color
purple10 = parse "#5C497E"

pink1 :: Color
pink1 = parse "#DB94B8"
pink2 :: Color
pink2 = rgb 236 189 210

{- Empty/null border. Makes for a flat look. -}
borderNone :: Css
borderNone = border solid (px 0) white

{- Timetable border -}
borderPink :: (Stroke -> Size Abs -> Color -> Css) -> Css
borderPink borderStroke = borderStroke solid (px 2) pink1

{- More node colours! -}
teal1 :: Color
teal1 = parse "#737A99"
orange1 :: Color
orange1 = parse "#1E7FCC"

blue1 :: Color
blue1 = parse "#261B2A"
blue2 :: Color
blue2 = parse "#336685"
blue3 :: Color
blue3 = parse "#437699"
blue4 :: Color
blue4 = parse "#5566F5"
blue5 :: Color
blue5 = parse "#A5A6F5"
blue6 :: Color
blue6 = rgb 184 231 249
blueFb :: Color
blueFb = rgb 59 89 152

red1 :: Color
red1 = parse "#C92343"
red2 :: Color
red2 = parse "#B91333"

red3 :: Color
red3 = rgb 215 117 70
red4 :: Color
red4 = rgb 195 97 50
red5 :: Color
red5 = rgb 221 189 189

green1 :: Color
green1 = rgb 170 228 164

dRed :: T.Text
dRed = "#D77546"

dGreen :: T.Text
dGreen = "#2E8B57"

dBlue :: T.Text
dBlue = "#437699"

dPurple :: T.Text
dPurple = "#46364A"

grey1 :: Color
grey1 = parse "#222"
grey2 :: Color
grey2 = parse "#dedede"
grey3 :: Color
grey3 = parse "#949494"
grey4 :: Color
grey4 = parse "#BABABA"
grey5 :: Color
grey5 = parse "#DCDCDC"

beige1 :: Color
beige1 = parse "#EBE8E4"

{- FCE count color. Currently unused. -}
fceCountColor :: Color
fceCountColor = parse "#66C2FF"

{- Graph styles -}

-- Node font size, in pixels
nodeFontSize :: Num a => a
nodeFontSize = 12

hybridFontSize :: Num a => a
hybridFontSize = 7

boolFontSize :: Num a => a
boolFontSize = 6

regionFontSize :: Num a => a
regionFontSize = 14

{- Complete Colour list According to Wikipedia: https://en.wikipedia.org/wiki/List_of_colors:_A%E2%80%93F -}

absoluteZero :: Color
absoluteZero = parse "#0048BA"

acajou :: Color
acajou = parse "#4C2F27"

acidGreen :: Color
acidGreen = parse "#B0BF1A"

aero :: Color
aero = parse "#7CB9E8"

aeroBlue :: Color
aeroBlue = parse "#C9FFE5"

africanViolet :: Color
africanViolet = parse "#B284BE"

airForceBlue :: Color
airForceBlue = parse "#00308F"

airSuperiorityBlue :: Color
airSuperiorityBlue = parse "#72A0C1"

alabamaCrimson :: Color
alabamaCrimson = parse "#AF002A"

alienArmpit :: Color
alienArmpit = parse "#84DE02"

alizarinCrimson :: Color
alizarinCrimson = parse "#E32636"

alloyOrange :: Color
alloyOrange = parse "#C46210"

amaranth :: Color
amaranth = parse "#E52B50"

amaranthDeepPurple :: Color
amaranthDeepPurple = parse "#9F2B68"

amaranthPink :: Color
amaranthPink = parse "#F19CBB"

amaranthPurple :: Color
amaranthPurple = parse "#AB274F"

amaranthRed :: Color
amaranthRed = parse "#D3212D"

amazon :: Color
amazon = parse "#3B7A57"

amber :: Color
amber = parse "#FF7E00"

amberSAE :: Color
amberSAE = parse "#FFBF00"

americanRose :: Color
americanRose = parse "#FF033E"

amethyst :: Color
amethyst = parse "#9966CC"

androidGreen :: Color
androidGreen = parse "#A4C639"

antiFlashWhite :: Color
antiFlashWhite = parse "#F2F3F4"

antiqueBrass :: Color
antiqueBrass = parse "#CD9575"

antiqueBronze :: Color
antiqueBronze = parse "#665D1E"

antiqueFuchsia :: Color
antiqueFuchsia = parse "#915C83"

antiqueRuby :: Color
antiqueRuby = parse "#841B2D"

ao :: Color
ao = parse "#008000"

appleGreen :: Color
appleGreen = parse "#8DB600"

apricot :: Color
apricot = parse "#FBCEB1"

arcticLime :: Color
arcticLime = parse "#D0FF14"

armyGreen :: Color
armyGreen = parse "#4B5320"

arsenic :: Color
arsenic = parse "#3B444B"

artichoke :: Color
artichoke = parse "#8F9779"

arylideYellow :: Color
arylideYellow = parse "#E9D66B"

ashGrey :: Color
ashGrey = parse "#B2BEB5"

asparagus :: Color
asparagus = parse "#87A96B"

atomicTangerine :: Color
atomicTangerine = parse "#FF9966"

auburn :: Color
auburn = parse "#A52A2A"

aureolin :: Color
aureolin = parse "#FDEE00"

auroMetalSaurus :: Color
auroMetalSaurus = parse "#6E7F80"

avocado :: Color
avocado = parse "#568203"

aztecGold :: Color
aztecGold = parse "#C39953"

azureishWhite :: Color
azureishWhite = parse "#DBE9F4"

azureMist :: Color
azureMist = parse "#F0FFFF"

azureWebColor :: Color
azureWebColor = parse "#F0FFFF"

babyBlue :: Color
babyBlue = parse "#89CFF0"

babyBlueEyes :: Color
babyBlueEyes = parse "#A1CAF1"

babyPink :: Color
babyPink = parse "#F4C2C2"

babyPowder :: Color
babyPowder = parse "#FEFEFA"

bakerMillerPink :: Color
bakerMillerPink = parse "#FF91AF"

ballBlue :: Color
ballBlue = parse "#21ABCD"

bananaMania :: Color
bananaMania = parse "#FAE7B5"

bananaYellow :: Color
bananaYellow = parse "#FFE135"

bangladeshGreen :: Color
bangladeshGreen = parse "#006A4E"

barbiePink :: Color
barbiePink = parse "#E0218A"

barnRed :: Color
barnRed = parse "#7C0A02"

batteryChargedBlue :: Color
batteryChargedBlue = parse "#1DACD6"

battleshipGrey :: Color
battleshipGrey = parse "#848482"

bazaar :: Color
bazaar = parse "#98777B"

bDazzledBlue :: Color
bDazzledBlue = parse "#2E5894"

beauBlue :: Color
beauBlue = parse "#BCD4E6"

beaver :: Color
beaver = parse "#9F8170"

begonia :: Color
begonia = parse "#FA6E79"

bigDipORuby :: Color
bigDipORuby = parse "#9C2542"

bigFootFeet :: Color
bigFootFeet = parse "#E88E5A"

bistre :: Color
bistre = parse "#3D2B1F"

bistreBrown :: Color
bistreBrown = parse "#967117"

bitterLemon :: Color
bitterLemon = parse "#CAE00D"

bitterLime :: Color
bitterLime = parse "#BFFF00"

bittersweet :: Color
bittersweet = parse "#FE6F5E"

bittersweetShimmer :: Color
bittersweetShimmer = parse "#BF4F51"

blackBean :: Color
blackBean = parse "#3D0C02"

blackCoral :: Color
blackCoral = parse "#54626F"

blackLeatherJacket :: Color
blackLeatherJacket = parse "#253529"

blackOlive :: Color
blackOlive = parse "#3B3C36"

blackShadows :: Color
blackShadows = parse "#BFAFB2"

blastOffBronze :: Color
blastOffBronze = parse "#A57164"

blazeOrange :: Color
blazeOrange = parse "#FF6700"

bleuDeFrance :: Color
bleuDeFrance = parse "#318CE7"

blizzardBlue :: Color
blizzardBlue = parse "#ACE5EE"

blond :: Color
blond = parse "#FAF0BE"

blueBell :: Color
blueBell = parse "#A2A2D0"

blueberry :: Color
blueberry = parse "#4F86F7"

blueBolt :: Color
blueBolt = parse "#00B9FB"

bluebonnet :: Color
bluebonnet = parse "#1C1CF0"

blueCrayola :: Color
blueCrayola = parse "#1F75FE"

blueGray :: Color
blueGray = parse "#6699CC"

blueGreen :: Color
blueGreen = parse "#0D98BA"

blueJeans :: Color
blueJeans = parse "#5DADEC"

blueLagoon :: Color
blueLagoon = parse "#ACE5EE"

blueMagentaViolet :: Color
blueMagentaViolet = parse "#553592"

blueMunsell :: Color
blueMunsell = parse "#0093AF"

blueNCS :: Color
blueNCS = parse "#0087BD"

bluePantone :: Color
bluePantone = parse "#0018A8"

bluePigment :: Color
bluePigment = parse "#333399"

blueRYB :: Color
blueRYB = parse "#0247FE"

blueSapphire :: Color
blueSapphire = parse "#126180"

blueYonder :: Color
blueYonder = parse "#5072A7"

bole :: Color
bole = parse "#79443B"

bondiBlue :: Color
bondiBlue = parse "#0095B6"

bone :: Color
bone = parse "#E3DAC9"

boogerBuster :: Color
boogerBuster = parse "#DDE26A"

bostonUniversityRed :: Color
bostonUniversityRed = parse "#CC0000"

bottleGreen :: Color
bottleGreen = parse "#006A4E"

boysenberry :: Color
boysenberry = parse "#873260"

brandeisBlue :: Color
brandeisBlue = parse "#0070FF"

brass :: Color
brass = parse "#B5A642"

brickRed :: Color
brickRed = parse "#CB4154"

brightCerulean :: Color
brightCerulean = parse "#1DACD6"

brightGreen :: Color
brightGreen = parse "#66FF00"

brightLavender :: Color
brightLavender = parse "#BF94E4"

brightLilac :: Color
brightLilac = parse "#D891EF"

brightMaroon :: Color
brightMaroon = parse "#C32148"

brightNavyBlue :: Color
brightNavyBlue = parse "#1974D2"

brightPink :: Color
brightPink = parse "#FF007F"

brightTurquoise :: Color
brightTurquoise = parse "#08E8DE"

brightUbe :: Color
brightUbe = parse "#D19FE8"

brightYellowCrayola :: Color
brightYellowCrayola = parse "#FFAA1D"

brilliantAzure :: Color
brilliantAzure = parse "#3399FF"

brilliantLavender :: Color
brilliantLavender = parse "#F4BBFF"

brilliantRose :: Color
brilliantRose = parse "#FF55A3"

brinkPink :: Color
brinkPink = parse "#FB607F"

britishRacingGreen :: Color
britishRacingGreen = parse "#004225"

bronze :: Color
bronze = parse "#CD7F32"

bronzeYellow :: Color
bronzeYellow = parse "#737000"

brownNose :: Color
brownNose = parse "#6B4423"

brownSugar :: Color
brownSugar = parse "#AF6E4D"

brownTraditional :: Color
brownTraditional = parse "#964B00"

brownWeb :: Color
brownWeb = parse "#A52A2A"

brownYellow :: Color
brownYellow = parse "#cc9966"

brunswickGreen :: Color
brunswickGreen = parse "#1B4D3E"

bubbleGum :: Color
bubbleGum = parse "#FFC1CC"

bubbles :: Color
bubbles = parse "#E7FEFF"

budGreen :: Color
budGreen = parse "#7BB661"

buff :: Color
buff = parse "#F0DC82"

bulgarianRose :: Color
bulgarianRose = parse "#480607"

burgundy :: Color
burgundy = parse "#800020"

burnishedBrown :: Color
burnishedBrown = parse "#A17A74"

burntOrange :: Color
burntOrange = parse "#CC5500"

burntSienna :: Color
burntSienna = parse "#E97451"

burntUmber :: Color
burntUmber = parse "#8A3324"

byzantine :: Color
byzantine = parse "#BD33A4"

byzantium :: Color
byzantium = parse "#702963"

cadetGrey :: Color
cadetGrey = parse "#91A3B0"

cadmiumGreen :: Color
cadmiumGreen = parse "#006B3C"

cadmiumOrange :: Color
cadmiumOrange = parse "#ED872D"

cadmiumRed :: Color
cadmiumRed = parse "#E30022"

cadmiumYellow :: Color
cadmiumYellow = parse "#FFF600"

cafeAuLait :: Color
cafeAuLait = parse "#A67B5B"

cafeNoir :: Color
cafeNoir = parse "#4B3621"

calPolyGreen :: Color
calPolyGreen = parse "#1E4D2B"

cambridgeBlue :: Color
cambridgeBlue = parse "#A3C1AD"

camel :: Color
camel = parse "#C19A6B"

cameoPink :: Color
cameoPink = parse "#EFBBCC"

camouflageGreen :: Color
camouflageGreen = parse "#78866B"

canary :: Color
canary = parse "#FFFF99"

canaryYellow :: Color
canaryYellow = parse "#FFEF00"

candyAppleRed :: Color
candyAppleRed = parse "#FF0800"

candyPink :: Color
candyPink = parse "#E4717A"

capri :: Color
capri = parse "#00BFFF"

caputMortuum :: Color
caputMortuum = parse "#592720"

cardinal :: Color
cardinal = parse "#C41E3A"

caribbeanGreen :: Color
caribbeanGreen = parse "#00CC99"

carmine :: Color
carmine = parse "#960018"

carmineMP :: Color
carmineMP = parse "#D70040"

carminePink :: Color
carminePink = parse "#EB4C42"

carmineRed :: Color
carmineRed = parse "#FF0038"

carnationPink :: Color
carnationPink = parse "#FFA6C9"

carnelian :: Color
carnelian = parse "#B31B1B"

carolinaBlue :: Color
carolinaBlue = parse "#56A0D3"

carrotOrange :: Color
carrotOrange = parse "#ED9121"

castletonGreen :: Color
castletonGreen = parse "#00563F"

catalinaBlue :: Color
catalinaBlue = parse "#062A78"

catawba :: Color
catawba = parse "#703642"

cedarChest :: Color
cedarChest = parse "#C95A49"

ceil :: Color
ceil = parse "#92A1CF"

celadon :: Color
celadon = parse "#ACE1AF"

celadonBlue :: Color
celadonBlue = parse "#007BA7"

celadonGreen :: Color
celadonGreen = parse "#2F847C"

celeste :: Color
celeste = parse "#B2FFFF"

celestialBlue :: Color
celestialBlue = parse "#4997D0"

cerise :: Color
cerise = parse "#DE3163"

cerisePink :: Color
cerisePink = parse "#EC3B83"

cerulean :: Color
cerulean = parse "#007BA7"

ceruleanBlue :: Color
ceruleanBlue = parse "#2A52BE"

ceruleanFrost :: Color
ceruleanFrost = parse "#6D9BC3"

cGBlue :: Color
cGBlue = parse "#007AA5"

cGRed :: Color
cGRed = parse "#E03C31"

chamoisee :: Color
chamoisee = parse "#A0785A"

champagne :: Color
champagne = parse "#F7E7CE"

champagnePink :: Color
champagnePink = parse "#F1DDCF"

charcoal :: Color
charcoal = parse "#36454F"

charlestonGreen :: Color
charlestonGreen = parse "#232B2B"

charmPink :: Color
charmPink = parse "#E68FAC"

chartreuseTraditional :: Color
chartreuseTraditional = parse "#DFFF00"

chartreuseWeb :: Color
chartreuseWeb = parse "#7FFF00"

cherry :: Color
cherry = parse "#DE3163"

cherryBlossomPink :: Color
cherryBlossomPink = parse "#FFB7C5"

chestnut :: Color
chestnut = parse "#954535"

chinaPink :: Color
chinaPink = parse "#DE6FA1"

chinaRose :: Color
chinaRose = parse "#A8516E"

chineseRed :: Color
chineseRed = parse "#AA381E"

chineseViolet :: Color
chineseViolet = parse "#856088"

chlorophyllGreen :: Color
chlorophyllGreen = parse "#4AFF00"

chocolateTraditional :: Color
chocolateTraditional = parse "#7B3F00"

chocolateWeb :: Color
chocolateWeb = parse "#D2691E"

chromeYellow :: Color
chromeYellow = parse "#FFA700"

cinereous :: Color
cinereous = parse "#98817B"

cinnabar :: Color
cinnabar = parse "#E34234"

cinnamon :: Color
cinnamon = parse "#D2691E"

cinnamonSatin :: Color
cinnamonSatin = parse "#CD607E"

citrine :: Color
citrine = parse "#E4D00A"

citron :: Color
citron = parse "#9FA91F"

claret :: Color
claret = parse "#7F1734"

classicRose :: Color
classicRose = parse "#FBCCE7"

cobaltBlue :: Color
cobaltBlue = parse "#0047AB"

cocoaBrown :: Color
cocoaBrown = parse "#D2691E"

coconut :: Color
coconut = parse "#965A3E"

coffee :: Color
coffee = parse "#6F4E37"

columbiaBlue :: Color
columbiaBlue = parse "#C4D8E2"

congoPink :: Color
congoPink = parse "#F88379"

coolBlack :: Color
coolBlack = parse "#002E63"

coolGrey :: Color
coolGrey = parse "#8C92AC"

copper :: Color
copper = parse "#B87333"

copperCrayola :: Color
copperCrayola = parse "#DA8A67"

copperPenny :: Color
copperPenny = parse "#AD6F69"

copperRed :: Color
copperRed = parse "#CB6D51"

copperRose :: Color
copperRose = parse "#996666"

coquelicot :: Color
coquelicot = parse "#FF3800"

coralPink :: Color
coralPink = parse "#F88379"

coralRed :: Color
coralRed = parse "#FF4040"

coralReef :: Color
coralReef = parse "#FD7C6E"

cordovan :: Color
cordovan = parse "#893F45"

cornellRed :: Color
cornellRed = parse "#B31B1B"

cosmicCobalt :: Color
cosmicCobalt = parse "#2E2D88"

cosmicLatte :: Color
cosmicLatte = parse "#FFF8E7"

cottonCandy :: Color
cottonCandy = parse "#FFBCD9"

coyoteBrown :: Color
coyoteBrown = parse "#81613C"

crimsonGlory :: Color
crimsonGlory = parse "#BE0032"

crimsonRed :: Color
crimsonRed = parse "#990000"

cultured :: Color
cultured = parse "#F5F5F5"

cyanAzure :: Color
cyanAzure = parse "#4E82B4"

cyanBlueAzure :: Color
cyanBlueAzure = parse "#4682BF"

cyanCobaltBlue :: Color
cyanCobaltBlue = parse "#28589C"

cyanCornflowerBlue :: Color
cyanCornflowerBlue = parse "#188BC2"

cyanProcess :: Color
cyanProcess = parse "#00B7EB"

cyberGrape :: Color
cyberGrape = parse "#58427C"

cyberYellow :: Color
cyberYellow = parse "#FFD300"

cyclamen :: Color
cyclamen = parse "#F56FA1"

daffodil :: Color
daffodil = parse "#FFFF31"

dandelion :: Color
dandelion = parse "#F0E130"

darkBlueGray :: Color
darkBlueGray = parse "#666699"

darkBrown :: Color
darkBrown = parse "#654321"

darkBrownTangelo :: Color
darkBrownTangelo = parse "#88654E"

darkByzantium :: Color
darkByzantium = parse "#5D3954"

darkCandyAppleRed :: Color
darkCandyAppleRed = parse "#A40000"

darkCerulean :: Color
darkCerulean = parse "#08457E"

darkChestnut :: Color
darkChestnut = parse "#986960"

darkCoral :: Color
darkCoral = parse "#CD5B45"

darkElectricBlue :: Color
darkElectricBlue = parse "#536878"

darkGunmetal :: Color
darkGunmetal = parse "#1F262A"

darkImperialBlue :: Color
darkImperialBlue = parse "#00416A"

darkImperialBlue2 :: Color
darkImperialBlue2 = parse "#6E6EF9"

darkJungleGreen :: Color
darkJungleGreen = parse "#1A2421"

darkLava :: Color
darkLava = parse "#483C32"

darkLavender :: Color
darkLavender = parse "#734F96"

darkLiver :: Color
darkLiver = parse "#534B4F"

darkLiverHorses :: Color
darkLiverHorses = parse "#543D37"

darkMediumGray :: Color
darkMediumGray = parse "#A9A9A9"

darkMidnightBlue :: Color
darkMidnightBlue = parse "#003366"

darkMossGreen :: Color
darkMossGreen = parse "#4A5D23"

darkPastelBlue :: Color
darkPastelBlue = parse "#779ECB"

darkPastelGreen :: Color
darkPastelGreen = parse "#03C03C"

darkPastelPurple :: Color
darkPastelPurple = parse "#966FD6"

darkPastelRed :: Color
darkPastelRed = parse "#C23B22"

darkPink :: Color
darkPink = parse "#E75480"

darkPowderBlue :: Color
darkPowderBlue = parse "#003399"

darkPuce :: Color
darkPuce = parse "#4F3A3C"

darkPurple :: Color
darkPurple = parse "#301934"

darkRaspberry :: Color
darkRaspberry = parse "#872657"

darkScarlet :: Color
darkScarlet = parse "#560319"

darkSienna :: Color
darkSienna = parse "#3C1414"

darkSkyBlue :: Color
darkSkyBlue = parse "#8CBED6"

darkSpringGreen :: Color
darkSpringGreen = parse "#177245"

darkTan :: Color
darkTan = parse "#918151"

darkTangerine :: Color
darkTangerine = parse "#FFA812"

darkTaupe :: Color
darkTaupe = parse "#483C32"

darkTerraCotta :: Color
darkTerraCotta = parse "#CC4E5C"

darkVanilla :: Color
darkVanilla = parse "#D1BEA8"

darkYellow :: Color
darkYellow = parse "#9B870C"

dartmouthGreen :: Color
dartmouthGreen = parse "#00703C"

davysGrey :: Color
davysGrey = parse "#555555"

debianRed :: Color
debianRed = parse "#D70A53"

deepAquamarine :: Color
deepAquamarine = parse "#40826D"

deepCarmine :: Color
deepCarmine = parse "#A9203E"

deepCarminePink :: Color
deepCarminePink = parse "#EF3038"

deepCarrotOrange :: Color
deepCarrotOrange = parse "#E9692C"

deepCerise :: Color
deepCerise = parse "#DA3287"

deepChampagne :: Color
deepChampagne = parse "#FAD6A5"

deepChestnut :: Color
deepChestnut = parse "#B94E48"

deepCoffee :: Color
deepCoffee = parse "#704241"

deepFuchsia :: Color
deepFuchsia = parse "#C154C1"

deepGreen :: Color
deepGreen = parse "#056608"

deepGreenCyanTurquoise :: Color
deepGreenCyanTurquoise = parse "#0E7C61"

deepJungleGreen :: Color
deepJungleGreen = parse "#004B49"

deepKoamaru :: Color
deepKoamaru = parse "#333366"

deepLemon :: Color
deepLemon = parse "#F5C71A"

deepLilac :: Color
deepLilac = parse "#9955BB"

deepMagenta :: Color
deepMagenta = parse "#CC00CC"

deepMaroon :: Color
deepMaroon = parse "#820000"

deepMauve :: Color
deepMauve = parse "#D473D4"

deepMossGreen :: Color
deepMossGreen = parse "#355E3B"

deepPeach :: Color
deepPeach = parse "#FFCBA4"

deepPuce :: Color
deepPuce = parse "#A95C68"

deepRed :: Color
deepRed = parse "#850101"

deepRuby :: Color
deepRuby = parse "#843F5B"

deepSaffron :: Color
deepSaffron = parse "#FF9933"

deepSpaceSparkle :: Color
deepSpaceSparkle = parse "#4A646C"

deepSpringBud :: Color
deepSpringBud = parse "#556B2F"

deepTaupe :: Color
deepTaupe = parse "#7E5E60"

deepTuscanRed :: Color
deepTuscanRed = parse "#66424D"

deepViolet :: Color
deepViolet = parse "#330066"

deer :: Color
deer = parse "#BA8759"

denim :: Color
denim = parse "#1560BD"

denimBlue :: Color
denimBlue = parse "#2243B6"

desaturatedCyan :: Color
desaturatedCyan = parse "#669999"

desert :: Color
desert = parse "#C19A6B"

desertSand :: Color
desertSand = parse "#EDC9AF"

desire :: Color
desire = parse "#EA3C53"

diamond :: Color
diamond = parse "#B9F2FF"

dingyDungeon :: Color
dingyDungeon = parse "#C53151"

dirt :: Color
dirt = parse "#9B7653"

dogwoodRose :: Color
dogwoodRose = parse "#D71868"

dollarBill :: Color
dollarBill = parse "#85BB65"

dolphinGray :: Color
dolphinGray = parse "#828E84"

donkeyBrown :: Color
donkeyBrown = parse "#664C28"

dukeBlue :: Color
dukeBlue = parse "#00009C"

dustStorm :: Color
dustStorm = parse "#E5CCC9"

dutchWhite :: Color
dutchWhite = parse "#EFDFBB"

earthYellow :: Color
earthYellow = parse "#E1A95F"

ebony :: Color
ebony = parse "#555D50"

ecru :: Color
ecru = parse "#C2B280"

eerieBlack :: Color
eerieBlack = parse "#1B1B1B"

eggplant :: Color
eggplant = parse "#614051"

eggshell :: Color
eggshell = parse "#F0EAD6"

egyptianBlue :: Color
egyptianBlue = parse "#1034A6"

electricBlue :: Color
electricBlue = parse "#7DF9FF"

electricCrimson :: Color
electricCrimson = parse "#FF003F"

electricCyan :: Color
electricCyan = parse "#00FFFF"

electricGreen :: Color
electricGreen = parse "#00FF00"

electricIndigo :: Color
electricIndigo = parse "#6F00FF"

electricLavender :: Color
electricLavender = parse "#F4BBFF"

electricLime :: Color
electricLime = parse "#CCFF00"

electricPurple :: Color
electricPurple = parse "#BF00FF"

electricUltramarine :: Color
electricUltramarine = parse "#3F00FF"

electricViolet :: Color
electricViolet = parse "#8F00FF"

electricYellow :: Color
electricYellow = parse "#FFFF33"

emerald :: Color
emerald = parse "#50C878"

eminence :: Color
eminence = parse "#6C3082"

englishGreen :: Color
englishGreen = parse "#1B4D3E"

englishLavender :: Color
englishLavender = parse "#B48395"

englishRed :: Color
englishRed = parse "#AB4B52"

englishVermillion :: Color
englishVermillion = parse "#CC474B"

englishViolet :: Color
englishViolet = parse "#563C5C"

etonBlue :: Color
etonBlue = parse "#96C8A2"

eucalyptus :: Color
eucalyptus = parse "#44D7A8"

fallow :: Color
fallow = parse "#C19A6B"

faluRed :: Color
faluRed = parse "#801818"

fandango :: Color
fandango = parse "#B53389"

fandangoPink :: Color
fandangoPink = parse "#DE5285"

fashionFuchsia :: Color
fashionFuchsia = parse "#F400A1"

fawn :: Color
fawn = parse "#E5AA70"

feldgrau :: Color
feldgrau = parse "#4D5D53"

feldspar :: Color
feldspar = parse "#FDD5B1"

fernGreen :: Color
fernGreen = parse "#4F7942"

ferrariRed :: Color
ferrariRed = parse "#FF2800"

fieldDrab :: Color
fieldDrab = parse "#6C541E"

fieryRose :: Color
fieryRose = parse "#FF5470"

fireEngineRed :: Color
fireEngineRed = parse "#CE2029"

flame :: Color
flame = parse "#E25822"

flamingoPink :: Color
flamingoPink = parse "#FC8EAC"

flattery :: Color
flattery = parse "#6B4423"

flavescent :: Color
flavescent = parse "#F7E98E"

flax :: Color
flax = parse "#EEDC82"

flirt :: Color
flirt = parse "#A2006D"

fluorescentOrange :: Color
fluorescentOrange = parse "#FFBF00"

fluorescentPink :: Color
fluorescentPink = parse "#FF1493"

fluorescentYellow :: Color
fluorescentYellow = parse "#CCFF00"

folly :: Color
folly = parse "#FF004F"

forestGreenTraditional :: Color
forestGreenTraditional = parse "#014421"

forestGreenWeb :: Color
forestGreenWeb = parse "#228B22"

frenchBeige :: Color
frenchBeige = parse "#A67B5B"

frenchBistre :: Color
frenchBistre = parse "#856D4D"

frenchBlue :: Color
frenchBlue = parse "#0072BB"

frenchFuchsia :: Color
frenchFuchsia = parse "#FD3F92"

frenchLilac :: Color
frenchLilac = parse "#86608E"

frenchLime :: Color
frenchLime = parse "#9EFD38"

frenchMauve :: Color
frenchMauve = parse "#D473D4"

frenchPink :: Color
frenchPink = parse "#FD6C9E"

frenchPlum :: Color
frenchPlum = parse "#811453"

frenchPuce :: Color
frenchPuce = parse "#4E1609"

frenchRaspberry :: Color
frenchRaspberry = parse "#C72C48"

frenchRose :: Color
frenchRose = parse "#F64A8A"

frenchSkyBlue :: Color
frenchSkyBlue = parse "#77B5FE"

frenchViolet :: Color
frenchViolet = parse "#8806CE"

frenchWine :: Color
frenchWine = parse "#AC1E44"

freshAir :: Color
freshAir = parse "#A6E7FF"

frostbite :: Color
frostbite = parse "#E936A7"

fuchsiaCrayola :: Color
fuchsiaCrayola = parse "#C154C1"

fuchsiaPink :: Color
fuchsiaPink = parse "#FF77FF"

fuchsiaPurple :: Color
fuchsiaPurple = parse "#CC397B"

fuchsiaRose :: Color
fuchsiaRose = parse "#C74375"

fulvous :: Color
fulvous = parse "#E48400"

fuzzyWuzzy :: Color
fuzzyWuzzy = parse "#CC6666"

gamboge :: Color
gamboge = parse "#E49B0F"

gambogeOrangeBrown :: Color
gambogeOrangeBrown = parse "#996600"

gargoyleGas :: Color
gargoyleGas = parse "#FFDF46"

genericViridian :: Color
genericViridian = parse "#007F66"

giantsClub :: Color
giantsClub = parse "#B05C52"

giantsOrange :: Color
giantsOrange = parse "#FE5A1D"

ginger :: Color
ginger = parse "#B06500"

glaucous :: Color
glaucous = parse "#6082B6"

glitter :: Color
glitter = parse "#E6E8FA"

glossyGrape :: Color
glossyGrape = parse "#AB92B3"

gOGreen :: Color
gOGreen = parse "#00AB66"

goldenBrown :: Color
goldenBrown = parse "#996515"

goldenPoppy :: Color
goldenPoppy = parse "#FCC200"

goldenYellow :: Color
goldenYellow = parse "#FFDF00"

goldFusion :: Color
goldFusion = parse "#85754E"

goldMetallic :: Color
goldMetallic = parse "#D4AF37"

goldWeb :: Color
goldWeb = parse "#FFD700"

graniteGray :: Color
graniteGray = parse "#676767"

grannySmithApple :: Color
grannySmithApple = parse "#A8E4A0"

grape :: Color
grape = parse "#6F2DA8"

grayAsparagus :: Color
grayAsparagus = parse "#465945"

grayBlue :: Color
grayBlue = parse "#8C92AC"

grayX11 :: Color
grayX11 = parse "#BEBEBE"

greenBlue :: Color
greenBlue = parse "#1164B4"

greenCrayola :: Color
greenCrayola = parse "#1CAC78"

greenCyan :: Color
greenCyan = parse "#009966"

greenHTML :: Color
greenHTML = parse "#008000"

greenLizard :: Color
greenLizard = parse "#A7F432"

greenMunsell :: Color
greenMunsell = parse "#00A877"

greenNCS :: Color
greenNCS = parse "#009F6B"

greenPantone :: Color
greenPantone = parse "#00AD43"

greenPigment :: Color
greenPigment = parse "#00A550"

greenRYB :: Color
greenRYB = parse "#66B032"

greenSheen :: Color
greenSheen = parse "#6EAEA1"

greenX11 :: Color
greenX11 = parse "#00FF00"

grizzly :: Color
grizzly = parse "#885818"

grullo :: Color
grullo = parse "#A99A86"

gunmetal :: Color
gunmetal = parse "#2a3439"

guppieGreen :: Color
guppieGreen = parse "#00FF7F"

halayaUbe :: Color
halayaUbe = parse "#663854"

hanBlue :: Color
hanBlue = parse "#446CCF"

hanPurple :: Color
hanPurple = parse "#5218FA"

hansaYellow :: Color
hansaYellow = parse "#E9D66B"

harlequin :: Color
harlequin = parse "#3FFF00"

harlequinGreen :: Color
harlequinGreen = parse "#46CB18"

harvardCrimson :: Color
harvardCrimson = parse "#C90016"

harvestGold :: Color
harvestGold = parse "#DA9100"

heartGold :: Color
heartGold = parse "#808000"

heatWave :: Color
heatWave = parse "#FF7A00"

heidelbergRed :: Color
heidelbergRed = parse "#960018"

heliotrope :: Color
heliotrope = parse "#DF73FF"

heliotropeGray :: Color
heliotropeGray = parse "#AA98A9"

heliotropeMagenta :: Color
heliotropeMagenta = parse "#AA00BB"

hollywoodCerise :: Color
hollywoodCerise = parse "#F400A1"

honoluluBlue :: Color
honoluluBlue = parse "#006DB0"

hookersGreen :: Color
hookersGreen = parse "#49796B"

hotMagenta :: Color
hotMagenta = parse "#FF1DCE"

hunterGreen :: Color
hunterGreen = parse "#355E3B"

iceberg :: Color
iceberg = parse "#71A6D2"

icterine :: Color
icterine = parse "#FCF75E"

iguanaGreen :: Color
iguanaGreen = parse "#71BC78"

illuminatingEmerald :: Color
illuminatingEmerald = parse "#319177"

imperial :: Color
imperial = parse "#602F6B"

imperialBlue :: Color
imperialBlue = parse "#002395"

imperialPurple :: Color
imperialPurple = parse "#66023C"

imperialRed :: Color
imperialRed = parse "#ED2939"

inchworm :: Color
inchworm = parse "#B2EC5D"

independence :: Color
independence = parse "#4C516D"

indiaGreen :: Color
indiaGreen = parse "#138808"

indianYellow :: Color
indianYellow = parse "#E3A857"

indigoDye :: Color
indigoDye = parse "#091F92"

indigoWeb :: Color
indigoWeb = parse "#4B0082"

infraRed :: Color
infraRed = parse "#FF496C"

interdimensionalBlue :: Color
interdimensionalBlue = parse "#360CCC"

internationalKleinBlue :: Color
internationalKleinBlue = parse "#002FA7"

internationalOrangeAerospace :: Color
internationalOrangeAerospace = parse "#FF4F00"

internationalOrangeEngineering :: Color
internationalOrangeEngineering = parse "#BA160C"

internationalOrangeGoldenGateBridge :: Color
internationalOrangeGoldenGateBridge = parse "#C0362C"

iris :: Color
iris = parse "#5A4FCF"

irresistible :: Color
irresistible = parse "#B3446C"

isabelline :: Color
isabelline = parse "#F4F0EC"

islamicGreen :: Color
islamicGreen = parse "#009000"

italianSkyBlue :: Color
italianSkyBlue = parse "#B2FFFF"

jade :: Color
jade = parse "#00A86B"

japaneseCarmine :: Color
japaneseCarmine = parse "#9D2933"

japaneseIndigo :: Color
japaneseIndigo = parse "#264348"

japaneseViolet :: Color
japaneseViolet = parse "#5B3256"

jasmine :: Color
jasmine = parse "#F8DE7E"

jasper :: Color
jasper = parse "#D73B3E"

jazzberryJam :: Color
jazzberryJam = parse "#A50B5E"

jellyBean :: Color
jellyBean = parse "#DA614E"

jet :: Color
jet = parse "#343434"

jonquil :: Color
jonquil = parse "#F4CA16"

jordyBlue :: Color
jordyBlue = parse "#8AB9F1"

juneBud :: Color
juneBud = parse "#BDDA57"

jungleGreen :: Color
jungleGreen = parse "#29AB87"

kellyGreen :: Color
kellyGreen = parse "#4CBB17"

kenyanCopper :: Color
kenyanCopper = parse "#7C1C05"

keppel :: Color
keppel = parse "#3AB09E"

keyLime :: Color
keyLime = parse "#E8F48C"

khakiHTML :: Color
khakiHTML = parse "#C3B091"

kiwi :: Color
kiwi = parse "#8EE53F"

kobe :: Color
kobe = parse "#882D17"

kobi :: Color
kobi = parse "#E79FC4"

kobicha :: Color
kobicha = parse "#6B4423"

kombuGreen :: Color
kombuGreen = parse "#354230"

kSUPurple :: Color
kSUPurple = parse "#512888"

kUCrimson :: Color
kUCrimson = parse "#E8000D"

languidLavender :: Color
languidLavender = parse "#D6CADD"

lapisLazuli :: Color
lapisLazuli = parse "#26619C"

laSalleGreen :: Color
laSalleGreen = parse "#087830"

laserLemon :: Color
laserLemon = parse "#FFFF66"

laurelGreen :: Color
laurelGreen = parse "#A9BA9D"

lava :: Color
lava = parse "#CF1020"

lavenderBlue :: Color
lavenderBlue = parse "#CCCCFF"

lavenderFloral :: Color
lavenderFloral = parse "#B57EDC"

lavenderGray :: Color
lavenderGray = parse "#C4C3D0"

lavenderIndigo :: Color
lavenderIndigo = parse "#9457EB"

lavenderMagenta :: Color
lavenderMagenta = parse "#EE82EE"

lavenderMist :: Color
lavenderMist = parse "#E6E6FA"

lavenderPink :: Color
lavenderPink = parse "#FBAED2"

lavenderPurple :: Color
lavenderPurple = parse "#967BB6"

lavenderRose :: Color
lavenderRose = parse "#FBA0E3"

lavenderWeb :: Color
lavenderWeb = parse "#E6E6FA"

lemonCurry :: Color
lemonCurry = parse "#CCA01D"

lemonGlacier :: Color
lemonGlacier = parse "#FDFF00"

lemonLime :: Color
lemonLime = parse "#E3FF00"

lemonMeringue :: Color
lemonMeringue = parse "#F6EABE"

lemonYellow :: Color
lemonYellow = parse "#FFF44F"

liberty :: Color
liberty = parse "#545AA7"

licorice :: Color
licorice = parse "#1A1110"

lightApricot :: Color
lightApricot = parse "#FDD5B1"

lightBrown :: Color
lightBrown = parse "#B5651D"

lightCarminePink :: Color
lightCarminePink = parse "#E66771"

lightCobaltBlue :: Color
lightCobaltBlue = parse "#88ACE0"

lightCornflowerBlue :: Color
lightCornflowerBlue = parse "#93CCEA"

lightCrimson :: Color
lightCrimson = parse "#F56991"

lightDeepPink :: Color
lightDeepPink = parse "#FF5CCD"

lightFrenchBeige :: Color
lightFrenchBeige = parse "#C8AD7F"

lightFuchsiaPink :: Color
lightFuchsiaPink = parse "#F984EF"

lightGrayishMagenta :: Color
lightGrayishMagenta = parse "#CC99CC"

lightHotPink :: Color
lightHotPink = parse "#FFB3DE"

lightKhaki :: Color
lightKhaki = parse "#F0E68C"

lightMediumOrchid :: Color
lightMediumOrchid = parse "#D39BCB"

lightMossGreen :: Color
lightMossGreen = parse "#ADDFAD"

lightOrchid :: Color
lightOrchid = parse "#E6A8D7"

lightPastelPurple :: Color
lightPastelPurple = parse "#B19CD9"

lightRedOchre :: Color
lightRedOchre = parse "#E97451"

lightSalmonPink :: Color
lightSalmonPink = parse "#FF9999"

lightTaupe :: Color
lightTaupe = parse "#B38B6D"

lightThulianPink :: Color
lightThulianPink = parse "#E68FAC"

lilac :: Color
lilac = parse "#C8A2C8"

lilacLuster :: Color
lilacLuster = parse "#AE98AA"

limerick :: Color
limerick = parse "#9DC209"

limeWeb :: Color
limeWeb = parse "#00FF00"

lincolnGreen :: Color
lincolnGreen = parse "#195905"

lion :: Color
lion = parse "#C19A6B"

liseranPurple :: Color
liseranPurple = parse "#DE6FA1"

littleBoyBlue :: Color
littleBoyBlue = parse "#6CA0DC"

liver :: Color
liver = parse "#674C47"

liverChestnut :: Color
liverChestnut = parse "#987456"

liverDogs :: Color
liverDogs = parse "#B86D29"

liverOrgan :: Color
liverOrgan = parse "#6C2E1F"

livid :: Color
livid = parse "#6699CC"

lumber :: Color
lumber = parse "#FFE4CD"

lust :: Color
lust = parse "#E62020"

maastrichtBlue :: Color
maastrichtBlue = parse "#001C3D"

macaroniAndCheese :: Color
macaroniAndCheese = parse "#FFBD88"

madderLake :: Color
madderLake = parse "#CC3336"

magentaCrayola :: Color
magentaCrayola = parse "#FF55A3"

magentaDye :: Color
magentaDye = parse "#CA1F7B"

magentaHaze :: Color
magentaHaze = parse "#9F4576"

magentaPantone :: Color
magentaPantone = parse "#D0417E"

magentaPink :: Color
magentaPink = parse "#CC338B"

magentaProcess :: Color
magentaProcess = parse "#FF0090"

magicMint :: Color
magicMint = parse "#AAF0D1"

magicPotion :: Color
magicPotion = parse "#FF4466"

magnolia :: Color
magnolia = parse "#F8F4FF"

mahogany :: Color
mahogany = parse "#C04000"

maize :: Color
maize = parse "#FBEC5D"

majorelleBlue :: Color
majorelleBlue = parse "#6050DC"

malachite :: Color
malachite = parse "#0BDA51"

manatee :: Color
manatee = parse "#979AAA"

mandarin :: Color
mandarin = parse "#F37A48"

mangoTango :: Color
mangoTango = parse "#FF8243"

mantis :: Color
mantis = parse "#74C365"

mardiGras :: Color
mardiGras = parse "#880085"

marigold :: Color
marigold = parse "#EAA221"

maroonCrayola :: Color
maroonCrayola = parse "#C32148"

maroonHTML :: Color
maroonHTML = parse "#800000"

maroonX11 :: Color
maroonX11 = parse "#B03060"

mauve :: Color
mauve = parse "#E0B0FF"

mauvelous :: Color
mauvelous = parse "#EF98AA"

mauveTaupe :: Color
mauveTaupe = parse "#915F6D"

maximumBlue :: Color
maximumBlue = parse "#47ABCC"

maximumBlueGreen :: Color
maximumBlueGreen = parse "#30BFBF"

maximumBluePurple :: Color
maximumBluePurple = parse "#ACACE6"

maximumGreen :: Color
maximumGreen = parse "#5E8C31"

maximumGreenYellow :: Color
maximumGreenYellow = parse "#D9E650"

maximumPurple :: Color
maximumPurple = parse "#733380"

maximumRed :: Color
maximumRed = parse "#D92121"

maximumRedPurple :: Color
maximumRedPurple = parse "#A63A79"

maximumYellow :: Color
maximumYellow = parse "#FAFA37"

maximumYellowRed :: Color
maximumYellowRed = parse "#F2BA49"

mayaBlue :: Color
mayaBlue = parse "#73C2FB"

mayGreen :: Color
mayGreen = parse "#4C9141"

meatBrown :: Color
meatBrown = parse "#E5B73B"

mediumCandyAppleRed :: Color
mediumCandyAppleRed = parse "#E2062C"

mediumCarmine :: Color
mediumCarmine = parse "#AF4035"

mediumChampagne :: Color
mediumChampagne = parse "#F3E5AB"

mediumElectricBlue :: Color
mediumElectricBlue = parse "#035096"

mediumJungleGreen :: Color
mediumJungleGreen = parse "#1C352D"

mediumLavenderMagenta :: Color
mediumLavenderMagenta = parse "#DDA0DD"

mediumPersianBlue :: Color
mediumPersianBlue = parse "#0067A5"

mediumRedViolet :: Color
mediumRedViolet = parse "#BB3385"

mediumRuby :: Color
mediumRuby = parse "#AA4069"

mediumSkyBlue :: Color
mediumSkyBlue = parse "#80DAEB"

mediumSpringBud :: Color
mediumSpringBud = parse "#C9DC87"

mediumTaupe :: Color
mediumTaupe = parse "#674C47"

mediumTuscanRed :: Color
mediumTuscanRed = parse "#79443B"

mediumVermilion :: Color
mediumVermilion = parse "#D9603B"

mellowApricot :: Color
mellowApricot = parse "#F8B878"

mellowYellow :: Color
mellowYellow = parse "#F8DE7E"

melon :: Color
melon = parse "#FDBCB4"

metallicSeaweed :: Color
metallicSeaweed = parse "#0A7E8C"

metallicSunburst :: Color
metallicSunburst = parse "#9C7C38"

metalPink :: Color
metalPink = parse "#FF00FD"

mexicanPink :: Color
mexicanPink = parse "#E4007C"

middleBlue :: Color
middleBlue = parse "#7ED4E6"

middleBlueGreen :: Color
middleBlueGreen = parse "#8DD9CC"

middleBluePurple :: Color
middleBluePurple = parse "#8B72BE"

middleGreen :: Color
middleGreen = parse "#4D8C57"

middleGreenYellow :: Color
middleGreenYellow = parse "#ACBF60"

middlePurple :: Color
middlePurple = parse "#D982B5"

middleRed :: Color
middleRed = parse "#E58E73"

middleRedPurple :: Color
middleRedPurple = parse "#8B8680"

middleRedPurple2 :: Color
middleRedPurple2 = parse "#A55353"

middleYellow :: Color
middleYellow = parse "#FFEB00"

middleYellowRed :: Color
middleYellowRed = parse "#ECB176"

midnightGreenEagleGreen :: Color
midnightGreenEagleGreen = parse "#004953"

mikadoYellow :: Color
mikadoYellow = parse "#FFC40C"

mimiPink :: Color
mimiPink = parse "#FFDAE9"

mindaro :: Color
mindaro = parse "#E3F988"

ming :: Color
ming = parse "#36747D"

minionYellow :: Color
minionYellow = parse "#F5E050"

mintGreen :: Color
mintGreen = parse "#98FF98"

mistyMoss :: Color
mistyMoss = parse "#BBB477"

modeBeige :: Color
modeBeige = parse "#967117"

moonstoneBlue :: Color
moonstoneBlue = parse "#73A9C2"

mordantRed19 :: Color
mordantRed19 = parse "#AE0C00"

mossGreen :: Color
mossGreen = parse "#8A9A5B"

mountainMeadow :: Color
mountainMeadow = parse "#30BA8F"

mountbattenPink :: Color
mountbattenPink = parse "#997A8D"

mSUGreen :: Color
mSUGreen = parse "#18453B"

mughalGreen :: Color
mughalGreen = parse "#306030"

mulberry :: Color
mulberry = parse "#C54B8C"

mummysTomb :: Color
mummysTomb = parse "#828E84"

mustard :: Color
mustard = parse "#FFDB58"

myrtleGreen :: Color
myrtleGreen = parse "#317873"

mystic :: Color
mystic = parse "#D65282"

mysticMaroon :: Color
mysticMaroon = parse "#AD4379"

nadeshikoPink :: Color
nadeshikoPink = parse "#F6ADC6"

napierGreen :: Color
napierGreen = parse "#2A8000"

naplesYellow :: Color
naplesYellow = parse "#FADA5E"

navyPurple :: Color
navyPurple = parse "#9457EB"

neonCarrot :: Color
neonCarrot = parse "#FFA343"

neonFuchsia :: Color
neonFuchsia = parse "#FE4164"

neonGreen :: Color
neonGreen = parse "#39FF14"

newCar :: Color
newCar = parse "#214FC6"

newYorkPink :: Color
newYorkPink = parse "#D7837F"

nickel :: Color
nickel = parse "#727472"

nonPhotoBlue :: Color
nonPhotoBlue = parse "#A4DDED"

northTexasGreen :: Color
northTexasGreen = parse "#059033"

nyanza :: Color
nyanza = parse "#E9FFDB"

oceanBlue :: Color
oceanBlue = parse "#4F42B5"

oceanBoatBlue :: Color
oceanBoatBlue = parse "#0077BE"

oceanGreen :: Color
oceanGreen = parse "#48BF91"

ochre :: Color
ochre = parse "#CC7722"

officeGreen :: Color
officeGreen = parse "#008000"

ogreOdor :: Color
ogreOdor = parse "#FD5240"

oldBurgundy :: Color
oldBurgundy = parse "#43302E"

oldGold :: Color
oldGold = parse "#CFB53B"

oldHeliotrope :: Color
oldHeliotrope = parse "#563C5C"

oldLavender :: Color
oldLavender = parse "#796878"

oldMauve :: Color
oldMauve = parse "#673147"

oldMossGreen :: Color
oldMossGreen = parse "#867E36"

oldRose :: Color
oldRose = parse "#C08081"

oldSilver :: Color
oldSilver = parse "#848482"

oliveDrab3 :: Color
oliveDrab3 = parse "#6B8E23"

oliveDrab7 :: Color
oliveDrab7 = parse "#3C341F"

olivine :: Color
olivine = parse "#9AB973"

onyx :: Color
onyx = parse "#353839"

operaMauve :: Color
operaMauve = parse "#B784A7"

orangeColorWheel :: Color
orangeColorWheel = parse "#FF7F00"

orangeCrayola :: Color
orangeCrayola = parse "#FF7538"

orangePantone :: Color
orangePantone = parse "#FF5800"

orangePeel :: Color
orangePeel = parse "#FF9F00"

orangeRYB :: Color
orangeRYB = parse "#FB9902"

orangeSoda :: Color
orangeSoda = parse "#FA5B3D"

orangeWeb :: Color
orangeWeb = parse "#FFA500"

orangeYellow :: Color
orangeYellow = parse "#F8D568"

orchidPink :: Color
orchidPink = parse "#F2BDCD"

oriolesOrange :: Color
oriolesOrange = parse "#FB4F14"

otterBrown :: Color
otterBrown = parse "#654321"

oUCrimsonRed :: Color
oUCrimsonRed = parse "#990000"

outerSpace :: Color
outerSpace = parse "#414A4C"

outrageousOrange :: Color
outrageousOrange = parse "#FF6E4A"

oxfordBlue :: Color
oxfordBlue = parse "#002147"

pacificBlue :: Color
pacificBlue = parse "#1CA9C9"

pakistanGreen :: Color
pakistanGreen = parse "#006600"

palatinateBlue :: Color
palatinateBlue = parse "#273BE2"

palatinatePurple :: Color
palatinatePurple = parse "#682860"

paleAqua :: Color
paleAqua = parse "#BCD4E6"

paleBlue :: Color
paleBlue = parse "#AFEEEE"

paleBrown :: Color
paleBrown = parse "#987654"

paleCarmine :: Color
paleCarmine = parse "#AF4035"

paleCerulean :: Color
paleCerulean = parse "#9BC4E2"

paleChestnut :: Color
paleChestnut = parse "#DDADAF"

paleCopper :: Color
paleCopper = parse "#DA8A67"

paleCornflowerBlue :: Color
paleCornflowerBlue = parse "#ABCDEF"

paleCyan :: Color
paleCyan = parse "#87D3F8"

paleLavender :: Color
paleLavender = parse "#DCD0FF"

paleMagenta :: Color
paleMagenta = parse "#F984E5"

paleMagentaPink :: Color
paleMagentaPink = parse "#FF99CC"

palePink :: Color
palePink = parse "#FADADD"

palePlum :: Color
palePlum = parse "#DDA0DD"

paleRedViolet :: Color
paleRedViolet = parse "#DB7093"

paleRobinEggBlue :: Color
paleRobinEggBlue = parse "#96DED1"

paleSilver :: Color
paleSilver = parse "#C9C0BB"

paleSpringBud :: Color
paleSpringBud = parse "#ECEBBD"

paleTaupe :: Color
paleTaupe = parse "#BC987E"

palmLeaf :: Color
palmLeaf = parse "#6F9940"

pansyPurple :: Color
pansyPurple = parse "#78184A"

paoloVeroneseGreen :: Color
paoloVeroneseGreen = parse "#009B7D"

paradisePink :: Color
paradisePink = parse "#E63E62"

parisGreen :: Color
parisGreen = parse "#50C878"

parrotPink :: Color
parrotPink = parse "#D998A0"

pastelBlue :: Color
pastelBlue = parse "#AEC6CF"

pastelBrown :: Color
pastelBrown = parse "#836953"

pastelGray :: Color
pastelGray = parse "#CFCFC4"

pastelGreen :: Color
pastelGreen = parse "#77DD77"

pastelMagenta :: Color
pastelMagenta = parse "#F49AC2"

pastelOrange :: Color
pastelOrange = parse "#FFB347"

pastelPink :: Color
pastelPink = parse "#DEA5A4"

pastelPurple :: Color
pastelPurple = parse "#B39EB5"

pastelRed :: Color
pastelRed = parse "#FF6961"

pastelViolet :: Color
pastelViolet = parse "#CB99C9"

pastelYellow :: Color
pastelYellow = parse "#FDFD96"

patriarch :: Color
patriarch = parse "#800080"

paynesGrey :: Color
paynesGrey = parse "#536878"

peachOrange :: Color
peachOrange = parse "#FFCC99"

peachYellow :: Color
peachYellow = parse "#FADFAD"

pear :: Color
pear = parse "#D1E231"

pearl :: Color
pearl = parse "#EAE0C8"

pearlAqua :: Color
pearlAqua = parse "#88D8C0"

pearlyPurple :: Color
pearlyPurple = parse "#B768A2"

peridot :: Color
peridot = parse "#E6E200"

periwinkle :: Color
periwinkle = parse "#CCCCFF"

permanentGeraniumLake :: Color
permanentGeraniumLake = parse "#E12C2C"

persianBlue :: Color
persianBlue = parse "#1C39BB"

persianGreen :: Color
persianGreen = parse "#00A693"

persianIndigo :: Color
persianIndigo = parse "#32127A"

persianOrange :: Color
persianOrange = parse "#D99058"

persianPink :: Color
persianPink = parse "#F77FBE"

persianPlum :: Color
persianPlum = parse "#701C1C"

persianRed :: Color
persianRed = parse "#CC3333"

persianRose :: Color
persianRose = parse "#FE28A2"

persimmon :: Color
persimmon = parse "#EC5800"

pewterBlue :: Color
pewterBlue = parse "#8BA8B7"

phlox :: Color
phlox = parse "#DF00FF"

phthaloBlue :: Color
phthaloBlue = parse "#000F89"

phthaloGreen :: Color
phthaloGreen = parse "#123524"

pictonBlue :: Color
pictonBlue = parse "#45B1E8"

pictorialCarmine :: Color
pictorialCarmine = parse "#C30B4E"

piggyPink :: Color
piggyPink = parse "#FDDDE6"

pineapple :: Color
pineapple = parse "#563C5C"

pineGreen :: Color
pineGreen = parse "#01796F"

pinkFlamingo :: Color
pinkFlamingo = parse "#FC74FD"

pinkLace :: Color
pinkLace = parse "#FFDDF4"

pinkLavender :: Color
pinkLavender = parse "#D8B2D1"

pinkOrange :: Color
pinkOrange = parse "#FF9966"

pinkPantone :: Color
pinkPantone = parse "#D74894"

pinkPearl :: Color
pinkPearl = parse "#E7ACCF"

pinkRaspberry :: Color
pinkRaspberry = parse "#980036"

pinkSherbet :: Color
pinkSherbet = parse "#F78FA7"

pistachio :: Color
pistachio = parse "#93C572"

pixiePowder :: Color
pixiePowder = parse "#391285"

platinum :: Color
platinum = parse "#E5E4E2"

plumpPurple :: Color
plumpPurple = parse "#5946B2"

plumWeb :: Color
plumWeb = parse "#DDA0DD"

polishedPine :: Color
polishedPine = parse "#5DA493"

pompAndPower :: Color
pompAndPower = parse "#86608E"

popstar :: Color
popstar = parse "#BE4F62"

portlandOrange :: Color
portlandOrange = parse "#FF5A36"

princessPerfume :: Color
princessPerfume = parse "#FF85CF"

princetonOrange :: Color
princetonOrange = parse "#F58025"

prune :: Color
prune = parse "#701C1C"

prussianBlue :: Color
prussianBlue = parse "#003153"

psychedelicPurple :: Color
psychedelicPurple = parse "#DF00FF"

puce :: Color
puce = parse "#CC8899"

puceRed :: Color
puceRed = parse "#722F37"

pullmanBrown :: Color
pullmanBrown = parse "#644117"

pullmanGreen :: Color
pullmanGreen = parse "#3B331C"

pumpkin :: Color
pumpkin = parse "#FF7518"

purpleHeart :: Color
purpleHeart = parse "#69359C"

purpleHTML :: Color
purpleHTML = parse "#800080"

purpleMountainMajesty :: Color
purpleMountainMajesty = parse "#9678B6"

purpleMunsell :: Color
purpleMunsell = parse "#9F00C5"

purpleNavy :: Color
purpleNavy = parse "#4E5180"

purplePizzazz :: Color
purplePizzazz = parse "#FE4EDA"

purplePlum :: Color
purplePlum = parse "#9C51B6"

purpleTaupe :: Color
purpleTaupe = parse "#50404D"

purpleX11 :: Color
purpleX11 = parse "#A020F0"

purpureus :: Color
purpureus = parse "#9A4EAE"

quartz :: Color
quartz = parse "#51484F"

queenBlue :: Color
queenBlue = parse "#436B95"

queenPink :: Color
queenPink = parse "#E8CCD7"

quickSilver :: Color
quickSilver = parse "#A6A6A6"

quinacridoneMagenta :: Color
quinacridoneMagenta = parse "#8E3A59"

rackley :: Color
rackley = parse "#5D8AA8"

radicalRed :: Color
radicalRed = parse "#FF355E"

raisinBlack :: Color
raisinBlack = parse "#242124"

rajah :: Color
rajah = parse "#FBAB60"

raspberry :: Color
raspberry = parse "#E30B5D"

raspberryGlace :: Color
raspberryGlace = parse "#915F6D"

raspberryPink :: Color
raspberryPink = parse "#E25098"

raspberryRose :: Color
raspberryRose = parse "#B3446C"

rawSienna :: Color
rawSienna = parse "#D68A59"

rawUmber :: Color
rawUmber = parse "#826644"

razzleDazzleRose :: Color
razzleDazzleRose = parse "#FF33CC"

razzmatazz :: Color
razzmatazz = parse "#E3256B"

razzmicBerry :: Color
razzmicBerry = parse "#8D4E85"

rebeccaPurple :: Color
rebeccaPurple = parse "#663399"

redBrown :: Color
redBrown = parse "#A52A2A"

redCrayola :: Color
redCrayola = parse "#EE204D"

redDevil :: Color
redDevil = parse "#860111"

redMunsell :: Color
redMunsell = parse "#F2003C"

redNCS :: Color
redNCS = parse "#C40233"

redOrange :: Color
redOrange = parse "#FF5349"

redPantone :: Color
redPantone = parse "#ED2939"

redPigment :: Color
redPigment = parse "#ED1C24"

redPurple :: Color
redPurple = parse "#E40078"

redRYB :: Color
redRYB = parse "#FE2712"

redSalsa :: Color
redSalsa = parse "#FD3A4A"

redViolet :: Color
redViolet = parse "#C71585"

redwood :: Color
redwood = parse "#A45A52"

regalia :: Color
regalia = parse "#522D80"

resolutionBlue :: Color
resolutionBlue = parse "#002387"

rhythm :: Color
rhythm = parse "#777696"

richBlack :: Color
richBlack = parse "#004040"

richBlackFOGRA29 :: Color
richBlackFOGRA29 = parse "#010B13"

richBlackFOGRA39 :: Color
richBlackFOGRA39 = parse "#010203"

richBrilliantLavender :: Color
richBrilliantLavender = parse "#F1A7FE"

richCarmine :: Color
richCarmine = parse "#D70040"

richElectricBlue :: Color
richElectricBlue = parse "#0892D0"

richLavender :: Color
richLavender = parse "#A76BCF"

richLilac :: Color
richLilac = parse "#B666D2"

richMaroon :: Color
richMaroon = parse "#B03060"

rifleGreen :: Color
rifleGreen = parse "#444C38"

roastCoffee :: Color
roastCoffee = parse "#704241"

robinEggBlue :: Color
robinEggBlue = parse "#00CCCC"

rocketMetallic :: Color
rocketMetallic = parse "#8A7F80"

romanSilver :: Color
romanSilver = parse "#838996"

roseBonbon :: Color
roseBonbon = parse "#F9429E"

roseDust :: Color
roseDust = parse "#9E5E6F"

roseEbony :: Color
roseEbony = parse "#674846"

roseGold :: Color
roseGold = parse "#B76E79"

roseMadder :: Color
roseMadder = parse "#E32636"

rosePink :: Color
rosePink = parse "#FF66CC"

roseQuartz :: Color
roseQuartz = parse "#AA98A9"

roseRed :: Color
roseRed = parse "#C21E56"

roseTaupe :: Color
roseTaupe = parse "#905D5D"

roseVale :: Color
roseVale = parse "#AB4E52"

rosewood :: Color
rosewood = parse "#65000B"

rossoCorsa :: Color
rossoCorsa = parse "#D40000"

royalAzure :: Color
royalAzure = parse "#0038A8"

royalFuchsia :: Color
royalFuchsia = parse "#CA2C92"

royalPurple :: Color
royalPurple = parse "#7851A9"

royalYellow :: Color
royalYellow = parse "#FADA5E"

ruber :: Color
ruber = parse "#CE4676"

rubineRed :: Color
rubineRed = parse "#D10056"

rubyRed :: Color
rubyRed = parse "#9B111E"

ruddy :: Color
ruddy = parse "#FF0028"

ruddyBrown :: Color
ruddyBrown = parse "#BB6528"

ruddyPink :: Color
ruddyPink = parse "#E18E96"

rufous :: Color
rufous = parse "#A81C07"

russet :: Color
russet = parse "#80461B"

russianGreen :: Color
russianGreen = parse "#679267"

russianViolet :: Color
russianViolet = parse "#32174D"

rust :: Color
rust = parse "#B7410E"

rustyRed :: Color
rustyRed = parse "#DA2C43"

sacramentoStateGreen :: Color
sacramentoStateGreen = parse "#00563F"

safetyOrange :: Color
safetyOrange = parse "#FF7800"

safetyYellow :: Color
safetyYellow = parse "#EED202"

saffron :: Color
saffron = parse "#F4C430"

sage :: Color
sage = parse "#BCB88A"

salmonPink :: Color
salmonPink = parse "#FF91A4"

sandDune :: Color
sandDune = parse "#967117"

sandstorm :: Color
sandstorm = parse "#ECD540"

sandyTan :: Color
sandyTan = parse "#FDD9B5"

sandyTaupe :: Color
sandyTaupe = parse "#967117"

sangria :: Color
sangria = parse "#92000A"

sapGreen :: Color
sapGreen = parse "#507D2A"

sapphire :: Color
sapphire = parse "#0F52BA"

sapphireBlue :: Color
sapphireBlue = parse "#0067A5"

sasquatchSocks :: Color
sasquatchSocks = parse "#FF4681"

satinSheenGold :: Color
satinSheenGold = parse "#CBA135"

scarlet :: Color
scarlet = parse "#FD0E35"

scarlet2 :: Color
scarlet2 = parse "#FF2400"

schaussPink :: Color
schaussPink = parse "#FF91AF"

schoolBusYellow :: Color
schoolBusYellow = parse "#FFD800"

screaminGreen :: Color
screaminGreen = parse "#66FF66"

seaBlue :: Color
seaBlue = parse "#006994"

seaFoamGreen :: Color
seaFoamGreen = parse "#9FE2BF"

sealBrown :: Color
sealBrown = parse "#59260B"

seaSerpent :: Color
seaSerpent = parse "#4BC7CF"

selectiveYellow :: Color
selectiveYellow = parse "#FFBA00"

shadow :: Color
shadow = parse "#8A795D"

shadowBlue :: Color
shadowBlue = parse "#778BA5"

shampoo :: Color
shampoo = parse "#FFCFF1"

shamrockGreen :: Color
shamrockGreen = parse "#009E60"

sheenGreen :: Color
sheenGreen = parse "#8FD400"

shimmeringBlush :: Color
shimmeringBlush = parse "#D98695"

shinyShamrock :: Color
shinyShamrock = parse "#5FA778"

shockingPink :: Color
shockingPink = parse "#FC0FC0"

shockingPinkCrayola :: Color
shockingPinkCrayola = parse "#FF6FFF"

silverChalice :: Color
silverChalice = parse "#ACACAC"

silverLakeBlue :: Color
silverLakeBlue = parse "#5D89BA"

silverPink :: Color
silverPink = parse "#C4AEAD"

silverSand :: Color
silverSand = parse "#BFC1C2"

sinopia :: Color
sinopia = parse "#CB410B"

sizzlingRed :: Color
sizzlingRed = parse "#FF3855"

sizzlingSunrise :: Color
sizzlingSunrise = parse "#FFDB00"

skobeloff :: Color
skobeloff = parse "#007474"

skyMagenta :: Color
skyMagenta = parse "#CF71AF"

slimyGreen :: Color
slimyGreen = parse "#299617"

smaltDarkPowderBlue :: Color
smaltDarkPowderBlue = parse "#003399"

smashedPumpkin :: Color
smashedPumpkin = parse "#FF6D3A"

smitten :: Color
smitten = parse "#C84186"

smokeyTopaz :: Color
smokeyTopaz = parse "#832A0D"

smokyBlack :: Color
smokyBlack = parse "#100C08"

smokyTopaz :: Color
smokyTopaz = parse "#933D41"

soap :: Color
soap = parse "#CEC8EF"

solidPink :: Color
solidPink = parse "#893843"

sonicSilver :: Color
sonicSilver = parse "#757575"

spaceCadet :: Color
spaceCadet = parse "#1D2951"

spanishBistre :: Color
spanishBistre = parse "#807532"

spanishBlue :: Color
spanishBlue = parse "#0070B8"

spanishCarmine :: Color
spanishCarmine = parse "#D10047"

spanishCrimson :: Color
spanishCrimson = parse "#E51A4C"

spanishGray :: Color
spanishGray = parse "#989898"

spanishGreen :: Color
spanishGreen = parse "#009150"

spanishOrange :: Color
spanishOrange = parse "#E86100"

spanishPink :: Color
spanishPink = parse "#F7BFBE"

spanishRed :: Color
spanishRed = parse "#E60026"

spanishSkyBlue :: Color
spanishSkyBlue = parse "#00FFFF"

spanishViolet :: Color
spanishViolet = parse "#4C2882"

spanishViridian :: Color
spanishViridian = parse "#007F5C"

spartanCrimson :: Color
spartanCrimson = parse "#9E1316"

spicyMix :: Color
spicyMix = parse "#8B5f4D"

spiroDiscoBall :: Color
spiroDiscoBall = parse "#0FC0FC"

springBud :: Color
springBud = parse "#A7FC00"

springFrost :: Color
springFrost = parse "#87FF2A"

starCommandBlue :: Color
starCommandBlue = parse "#007BB8"

steelPink :: Color
steelPink = parse "#CC33CC"

steelTeal :: Color
steelTeal = parse "#5F8A8B"

stilDeGrainYellow :: Color
stilDeGrainYellow = parse "#FADA5E"

stizza :: Color
stizza = parse "#990000"

stormcloud :: Color
stormcloud = parse "#4F666A"

stPatricksBlue :: Color
stPatricksBlue = parse "#23297A"

straw :: Color
straw = parse "#E4D96F"

strawberry :: Color
strawberry = parse "#FC5A8D"

sugarPlum :: Color
sugarPlum = parse "#914E75"

sunburntCyclops :: Color
sunburntCyclops = parse "#FF404C"

sunglow :: Color
sunglow = parse "#FFCC33"

sunny :: Color
sunny = parse "#F2F27A"

sunray :: Color
sunray = parse "#E3AB57"

sunset :: Color
sunset = parse "#FAD6A5"

sunsetOrange :: Color
sunsetOrange = parse "#FD5E53"

superPink :: Color
superPink = parse "#CF6BA9"

sweetBrown :: Color
sweetBrown = parse "#A83731"

tangelo :: Color
tangelo = parse "#F94D00"

tangerine :: Color
tangerine = parse "#F28500"

tangerineYellow :: Color
tangerineYellow = parse "#FFCC00"

tangoPink :: Color
tangoPink = parse "#E4717A"

tartOrange :: Color
tartOrange = parse "#FB4D46"

taupe :: Color
taupe = parse "#483C32"

taupeGray :: Color
taupeGray = parse "#8B8589"

teaGreen :: Color
teaGreen = parse "#D0F0C0"

tealBlue :: Color
tealBlue = parse "#367588"

tealDeer :: Color
tealDeer = parse "#99E6B3"

tealGreen :: Color
tealGreen = parse "#00827F"

teaRose :: Color
teaRose = parse "#F4C2C2"

teaRose2 :: Color
teaRose2 = parse "#F88379"

telemagenta :: Color
telemagenta = parse "#CF3476"

tenne :: Color
tenne = parse "#CD5700"

terraCotta :: Color
terraCotta = parse "#E2725B"

thulianPink :: Color
thulianPink = parse "#DE6FA1"

tickleMePink :: Color
tickleMePink = parse "#FC89AC"

tiffanyBlue :: Color
tiffanyBlue = parse "#0ABAB5"

tigersEye :: Color
tigersEye = parse "#E08D3C"

timberwolf :: Color
timberwolf = parse "#DBD7D2"

titaniumYellow :: Color
titaniumYellow = parse "#EEE600"

toolbox :: Color
toolbox = parse "#746CC0"

topaz :: Color
topaz = parse "#FFC87C"

tractorRed :: Color
tractorRed = parse "#FD0E35"

trolleyGrey :: Color
trolleyGrey = parse "#808080"

tropicalRainForest :: Color
tropicalRainForest = parse "#00755E"

tropicalViolet :: Color
tropicalViolet = parse "#CDA4DE"

trueBlue :: Color
trueBlue = parse "#0073CF"

tuftsBlue :: Color
tuftsBlue = parse "#417DC1"

tulip :: Color
tulip = parse "#FF878D"

tumbleweed :: Color
tumbleweed = parse "#DEAA88"

turkishRose :: Color
turkishRose = parse "#B57281"

turquoiseBlue :: Color
turquoiseBlue = parse "#00FFEF"

turquoiseGreen :: Color
turquoiseGreen = parse "#A0D6B4"

turquoiseSurf :: Color
turquoiseSurf = parse "#00C5CD"

turtleGreen :: Color
turtleGreen = parse "#8A9A5B"

tuscan :: Color
tuscan = parse "#FAD6A5"

tuscanBrown :: Color
tuscanBrown = parse "#6F4E37"

tuscanRed :: Color
tuscanRed = parse "#7C4848"

tuscanTan :: Color
tuscanTan = parse "#A67B5B"

tuscany :: Color
tuscany = parse "#C09999"

twilightLavender :: Color
twilightLavender = parse "#8A496B"

tyrianPurple :: Color
tyrianPurple = parse "#66023C"

uABlue :: Color
uABlue = parse "#0033AA"

uARed :: Color
uARed = parse "#D9004C"

ube :: Color
ube = parse "#8878C3"

uCLABlue :: Color
uCLABlue = parse "#536895"

uCLAGold :: Color
uCLAGold = parse "#FFB300"

uFOGreen :: Color
uFOGreen = parse "#3CD070"

ultramarine :: Color
ultramarine = parse "#3F00FF"

ultramarineBlue :: Color
ultramarineBlue = parse "#4166F5"

ultraPink :: Color
ultraPink = parse "#FF6FFF"

ultraRed :: Color
ultraRed = parse "#FC6C85"

umber :: Color
umber = parse "#635147"

unbleachedSilk :: Color
unbleachedSilk = parse "#FFDDCA"

unitedNationsBlue :: Color
unitedNationsBlue = parse "#5B92E5"

universityOfCaliforniaGold :: Color
universityOfCaliforniaGold = parse "#B78727"

universityOfTennesseeOrange :: Color
universityOfTennesseeOrange = parse "#F77F00"

unmellowYellow :: Color
unmellowYellow = parse "#FFFF66"

uPForestGreen :: Color
uPForestGreen = parse "#014421"

uPMaroon :: Color
uPMaroon = parse "#7B1113"

upsdellRed :: Color
upsdellRed = parse "#AE2029"

urobilin :: Color
urobilin = parse "#E1AD21"

uSAFABlue :: Color
uSAFABlue = parse "#004F98"

uSCCardinal :: Color
uSCCardinal = parse "#990000"

uSCGold :: Color
uSCGold = parse "#FFCC00"

utahCrimson :: Color
utahCrimson = parse "#D3003F"

vanDykeBrown :: Color
vanDykeBrown = parse "#664228"

vanilla :: Color
vanilla = parse "#F3E5AB"

vanillaIce :: Color
vanillaIce = parse "#F38FA9"

vegasGold :: Color
vegasGold = parse "#C5B358"

venetianRed :: Color
venetianRed = parse "#C80815"

verdigris :: Color
verdigris = parse "#43B3AE"

vermilion :: Color
vermilion = parse "#D9381E"

vermilion2 :: Color
vermilion2 = parse "#E34234"

veronica :: Color
veronica = parse "#A020F0"

veryLightAzure :: Color
veryLightAzure = parse "#74BBFB"

veryLightBlue :: Color
veryLightBlue = parse "#6666FF"

veryLightMalachiteGreen :: Color
veryLightMalachiteGreen = parse "#64E986"

veryLightTangelo :: Color
veryLightTangelo = parse "#FFB077"

veryPaleOrange :: Color
veryPaleOrange = parse "#FFDFBF"

veryPaleYellow :: Color
veryPaleYellow = parse "#FFFFBF"

vESPIERose :: Color
vESPIERose = parse "#FF0080"

violetBlue :: Color
violetBlue = parse "#324AB2"

violetColorWheel :: Color
violetColorWheel = parse "#7F00FF"

violetRYB :: Color
violetRYB = parse "#8601AF"

violetWeb :: Color
violetWeb = parse "#EE82EE"

viridian :: Color
viridian = parse "#40826D"

viridianGreen :: Color
viridianGreen = parse "#009698"

vistaBlue :: Color
vistaBlue = parse "#7C9ED9"

vividAmber :: Color
vividAmber = parse "#CC9900"

vividAuburn :: Color
vividAuburn = parse "#922724"

vividBurgundy :: Color
vividBurgundy = parse "#9F1D35"

vividCerise :: Color
vividCerise = parse "#DA1D81"

vividCerulean :: Color
vividCerulean = parse "#00AAEE"

vividCrimson :: Color
vividCrimson = parse "#CC0033"

vividGamboge :: Color
vividGamboge = parse "#FF9900"

vividLimeGreen :: Color
vividLimeGreen = parse "#A6D608"

vividMalachite :: Color
vividMalachite = parse "#00CC33"

vividMulberry :: Color
vividMulberry = parse "#B80CE3"

vividOrange :: Color
vividOrange = parse "#FF5F00"

vividOrangePeel :: Color
vividOrangePeel = parse "#FFA000"

vividOrchid :: Color
vividOrchid = parse "#CC00FF"

vividRaspberry :: Color
vividRaspberry = parse "#FF006C"

vividRed :: Color
vividRed = parse "#F70D1A"

vividRedTangelo :: Color
vividRedTangelo = parse "#DF6124"

vividSkyBlue :: Color
vividSkyBlue = parse "#00CCFF"

vividTangelo :: Color
vividTangelo = parse "#F07427"

vividTangerine :: Color
vividTangerine = parse "#FFA089"

vividVermilion :: Color
vividVermilion = parse "#E56024"

vividViolet :: Color
vividViolet = parse "#9F00FF"

vividYellow :: Color
vividYellow = parse "#FFE302"

volt :: Color
volt = parse "#CEFF00"

wageningenGreen :: Color
wageningenGreen = parse "#34B233"

warmBlack :: Color
warmBlack = parse "#004242"

waterspout :: Color
waterspout = parse "#A4F4F9"

weldonBlue :: Color
weldonBlue = parse "#7C98AB"

wenge :: Color
wenge = parse "#645452"

wildBlueYonder :: Color
wildBlueYonder = parse "#A2ADD0"

wildOrchid :: Color
wildOrchid = parse "#D470A2"

wildStrawberry :: Color
wildStrawberry = parse "#FF43A4"

wildWatermelon :: Color
wildWatermelon = parse "#FC6C85"

willpowerOrange :: Color
willpowerOrange = parse "#FD5800"

windsorTan :: Color
windsorTan = parse "#A75502"

wine :: Color
wine = parse "#722F37"

wineDregs :: Color
wineDregs = parse "#673147"

wintergreenDream :: Color
wintergreenDream = parse "#56887D"

winterSky :: Color
winterSky = parse "#FF007C"

winterWizard :: Color
winterWizard = parse "#A0E6FF"

wisteria :: Color
wisteria = parse "#C9A0DC"

woodBrown :: Color
woodBrown = parse "#C19A6B"

xanadu :: Color
xanadu = parse "#738678"

yaleBlue :: Color
yaleBlue = parse "#0F4D92"

yankeesBlue :: Color
yankeesBlue = parse "#1C2841"

yellowCrayola :: Color
yellowCrayola = parse "#FCE883"

yellowMunsell :: Color
yellowMunsell = parse "#EFCC00"

yellowNCS :: Color
yellowNCS = parse "#FFD300"

yellowOrange :: Color
yellowOrange = parse "#FFAE42"

yellowPantone :: Color
yellowPantone = parse "#FEDF00"

yellowProcess :: Color
yellowProcess = parse "#FFEF00"

yellowRose :: Color
yellowRose = parse "#FFF000"

yellowRYB :: Color
yellowRYB = parse "#FEFE33"

yellowSunshine :: Color
yellowSunshine = parse "#FFF700"

zaffre :: Color
zaffre = parse "#0014A8"

zinnwalditeBrown :: Color
zinnwalditeBrown = parse "#2C1608"

zomp :: Color
zomp = parse "#39A78E"
