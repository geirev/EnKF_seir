#!MC 1410
$!OpenLayout  "/home/geve/Dropbox/EnKF_seir/run/seirD2.lay"

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
$!XYLineAxis YDetail 1 {RangeMax = 400}
$!XYLineAxis YDetail 1 {RangeMin = 0}
$!XYLineAxis XDetail 1 {RangeMin = 43891}
$!XYLineAxis XDetail 1 {RangeMax = 43991}
$!RedrawAll 
$!ExportSetup ExportFName = '/home/geve/Dropbox/EnKF_seir/run/HD.eps'
$!Export 
  ExportRegion = AllFrames


$!FrameControl ActivateByNumber
  Frame = 1
$!ActiveLineMaps -= [1-1412]
$!ActiveLineMaps -= [301-500]
$!ActiveLineMaps += [1001-1200,1406,1408]
$!FrameControl ActivateByNumber
  Frame = 2
$!ActiveLineMaps -= [1-2]
$!XYLineAxis YDetail 1 {RangeMax = 20000}
$!XYLineAxis YDetail 1 {RangeMin = 0}
$!XYLineAxis XDetail 1 {RangeMin = 43891}
$!XYLineAxis XDetail 1 {RangeMax = 43991}
$!RedrawAll 
$!ExportSetup ExportFName = '/home/geve/Dropbox/EnKF_seir/run/IE.eps'
$!Export 
  ExportRegion = AllFrames


$!FrameControl ActivateByNumber
  Frame = 1
$!ActiveLineMaps -= [1-1412]
$!ActiveLineMaps -= [101-300]
$!ActiveLineMaps += [801-1000,1410,1412]
$!FrameControl ActivateByNumber
  Frame = 2
$!XYLineAxis YDetail 1 {RangeMax = 70000}
$!XYLineAxis YDetail 1 {RangeMin = 0}
$!XYLineAxis XDetail 1 {RangeMin = 43891}
$!XYLineAxis XDetail 1 {RangeMax = 43991}
$!RedrawAll 
$!ExportSetup ExportFName = '/home/geve/Dropbox/EnKF_seir/run/CR.eps'
$!Export 
  ExportRegion = AllFrames



$!OpenLayout  "/home/geve/Dropbox/EnKF_seir/run/bigD.lay"
$!RedrawAll 
$!ExportSetup ExportFName = '/home/geve/Dropbox/EnKF_seir/run/BIG.eps'
$!Export 
  ExportRegion = AllFrames
