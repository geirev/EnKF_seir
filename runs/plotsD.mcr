#!MC 1410
$!OpenLayout  "./solutions.lay"

$!PrintSetup Palette = Color
$!ExportSetup ExportFormat = EPS
$!ExportSetup EPSPreviewImage{ImageType = None}


$!FrameControl ActivateByNumber
  Frame = 1
$!ActiveLineMaps -= [1-1412]
$!ActiveLineMaps -= [501-700]
$!ActiveLineMaps += [1201-1400,1402,1404]
$!FrameControl ActivateByNumber
  Frame = 2
$!ActiveLineMaps += [1-2]
$!ActiveLineMaps -= [3]
$!XYLineAxis YDetail 1 {RangeMin = 0}
$!XYLineAxis XDetail 1 {RangeMin = XXXXX}
$!XYLineAxis XDetail 1 {RangeMax = YYYYY}
$!RedrawAll 
$!ExportSetup ExportFName = './HD.eps'
$!Export 
  ExportRegion = AllFrames
$!PAUSE "HD"


$!FrameControl ActivateByNumber
  Frame = 1
$!ActiveLineMaps -= [1-1412]
$!ActiveLineMaps -= [301-500]
$!ActiveLineMaps += [1001-1200,1406,1408]
$!View Fit
$!FrameControl ActivateByNumber
  Frame = 2
$!ActiveLineMaps -= [1-3]
#$!XYLineAxis YDetail 1 {RangeMax = 20000}
$!XYLineAxis YDetail 1 {RangeMin = 0}
$!XYLineAxis XDetail 1 {RangeMin = XXXXX}
$!XYLineAxis XDetail 1 {RangeMax = YYYYY}
$!RedrawAll 
$!ExportSetup ExportFName = './IE.eps'
$!Export 
  ExportRegion = AllFrames
$!PAUSE "IE"


$!FrameControl ActivateByNumber
  Frame = 1
$!ActiveLineMaps -= [1-1412]
$!ActiveLineMaps -= [101-300]
$!ActiveLineMaps += [801-1000,1410,1412]
$!View Fit
$!FrameControl ActivateByNumber
  Frame = 2
$!ActiveLineMaps -= [1-2]
$!ActiveLineMaps += [3]
$!XYLineAxis YDetail 1 {RangeMin = 0}
#$!XYLineAxis YDetail 1 {RangeMax = 70000}
$!XYLineAxis XDetail 1 {RangeMin = XXXXX}
$!XYLineAxis XDetail 1 {RangeMax = YYYYY}
$!RedrawAll 
$!ExportSetup ExportFName = './CR.eps'
$!Export 
  ExportRegion = AllFrames
$!PAUSE "CR"



$!OpenLayout  "./rens.lay"
$!XYLineAxis YDetail 1 {RangeMin = 0}
$!XYLineAxis YDetail 1 {RangeMax = 6}
$!XYLineAxis XDetail 1 {RangeMin = XXXXX}
$!XYLineAxis XDetail 1 {RangeMax = YYYYY}
$!RedrawAll 
$!ExportSetup ExportFName = './RENS.eps'
$!Export 
  ExportRegion = AllFrames
$!PAUSE "RENS"
