{-|
    Module      : Css.Search
    Description : Defines CSS for the search page.

There is a distinction between the search bar, and the search page. The grid
page has a search bar, but it is not the search page. The search page is found
by clicking the search button at the top of Courseography.
-}
module Css.Search
    (searchStyles) where

import Clay
import Css.Constants

-- |Defines the CSS for the search page.
searchStyles :: Css
searchStyles = do
    "#timetableSearch" ? do
        alignCenter
    "#filter" ? do
        width (px 200)
        border solid (px 1) black
        fontFamily ["Trebuchet MS", "Arial"] [sansSerif]
        marginLeft (px 10)
        alignCenter
    "#deptSelect" ? do
        marginLeft (px 10)
        alignCenter
    "#timetableContainer" ? do
        width (px 1016)
        height100
        marginTop (em 1)
        marginLeft auto
        marginRight auto
    "#timetableMain" ? do
        border solid (px 2) black
        "border-spacing" -: "0"
        "table-layout" -: "fixed"
        backgroundColor grey3
        td <> th ? do
            margin0
            padding0
            "border-spacing" -: "0"
            borderStyle solid
            borderColor black
            borderWidth4 (px 2) (px 2) (px 1) (px 2)
            verticalAlign vAlignTop
            ".sessionHeader" & do
                alignCenter
                fontWeight bold
                fontSize (em 1.1)
                backgroundColor blue5
            ".timetableSection" & do
                width (px 50)
                backgroundColor red5
                alignCenter
            ".timetableTime" & do
                width (px 110)
                backgroundColor blue6
                paddingLeft (px 5)
                paddingRight (px 5)
            ".timetableInstructor" & do
                width (px 150)
                backgroundColor purple3
                paddingLeft (px 5)
            ".timetableCap" & do
                width (px 95)
                backgroundColor green1
                alignCenter
            ".timetableWait" & do
                width (px 40)
                backgroundColor pink2
                alignCenter
        ".courseTable" ? do
            minHeight (px 23)
            "border-spacing" -: "0"
            "table-layout" -: "fixed"
            tr <> td <> th ? do
                borderStyle none
        ".timetableCourseName" ? do
            fontSize (em 1.05)
            minWidth (px 90)
            alignCenter
            backgroundColor blue5
        ".FOffering" <> ".SOffering" <> ".YOffering" ? do
            width (px 460)
            table <? do
                width (px 460)
                margin0
                padding0
                borderStyle none
        ".YOffering .courseTable" ? do
            marginLeft auto
            marginRight auto
