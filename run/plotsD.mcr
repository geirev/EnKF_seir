#!MC 1410
$!OpenLayout  "/home/geve/Dropbox/EnKF_seir/run/seirD2.lay"

$!PrintSetup Palette = Color
$!ExportSetup ExportFormat = EPS
$!ExportSetup EPSPreviewImage{ImageType = None}


$!FrameControl ActivateByNumber
  Frame = 1
$!ActiveLineMaps -= [1-1606]
$!ActiveLineMaps += [501-700,1201-1400,1401-1404]
$!FrameControl ActivateByNumber
  Frame = 2
$!ActiveLineMaps += [1-2]



$!XYLineAxis YDetail 1 {RangeMax = 400}
$!XYLineAxis YDetail 1 {RangeMin = 0}
$!XYLineAxis XDetail 1 {RangeMin = 43891}
$!XYLineAxis XDetail 1 {RangeMax = 43991}
$!ExportSetup ExportFName = '/home/geve/Dropbox/EnKF_seir/run/HD.eps'
$!Export 
  ExportRegion = AllFrames


$!FrameControl ActivateByNumber
  Frame = 1
$!ActiveLineMaps -= [1-1404]
$!ActiveLineMaps += [301-500,1001-1200]
$!FrameControl ActivateByNumber
  Frame = 2
$!ActiveLineMaps -= [1-2]

$!XYLineAxis YDetail 1 {RangeMax = 20000}
$!XYLineAxis YDetail 1 {RangeMin = 0}
$!XYLineAxis XDetail 1 {RangeMin = 43891}
$!XYLineAxis XDetail 1 {RangeMax = 43991}
$!ExportSetup ExportFName = '/home/geve/Dropbox/EnKF_seir/run/IE.eps'
$!Export 
  ExportRegion = AllFrames


$!FrameControl ActivateByNumber
  Frame = 1
$!ActiveLineMaps -= [1-1404]
$!ActiveLineMaps += [101-300,801-1000]
$!XYLineAxis YDetail 1 {RangeMax = 70000}
$!XYLineAxis YDetail 1 {RangeMin = 0}
$!XYLineAxis XDetail 1 {RangeMin = 43891}
$!XYLineAxis XDetail 1 {RangeMax = 43991}
$!ExportSetup ExportFName = '/home/geve/Dropbox/EnKF_seir/run/CR.eps'
$!Export 
  ExportRegion = AllFrames






$!OpenLayout  "/home/geve/Dropbox/EnKF_seir/run/bigD.lay"
$!ExportSetup ExportFName = '/home/geve/Dropbox/EnKF_seir/run/BIG.eps'
$!Export 
  ExportRegion = AllFrames
