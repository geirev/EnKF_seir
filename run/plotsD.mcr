#!MC 1410
$!OpenLayout  "/home/geve/Dropbox/EnKF_seir/run/seirD.lay"

$!PrintSetup Palette = Color
$!ExportSetup ExportFormat = EPS
$!ExportSetup EPSPreviewImage{ImageType = None}


$!ActiveLineMaps -= [1-1606]
$!ActiveLineMaps += [1401-1600,601-800,1601-1606]
$!XYLineAxis YDetail 1 {RangeMax = 400}
$!XYLineAxis YDetail 1 {RangeMin = 0}
$!XYLineAxis XDetail 1 {RangeMin = 43891}
$!XYLineAxis XDetail 1 {RangeMax = 43991}
$!ExportSetup ExportFName = '/home/geve/Dropbox/EnKF_seir/run/HD.eps'
$!Export 
  ExportRegion = AllFrames


$!ActiveLineMaps -= [1-1606]
$!ActiveLineMaps += [301-500,1101-1300]
$!XYLineAxis YDetail 1 {RangeMax = 30000}
$!XYLineAxis YDetail 1 {RangeMin = 0}
$!XYLineAxis XDetail 1 {RangeMin = 43891}
$!XYLineAxis XDetail 1 {RangeMax = 43991}
$!ExportSetup ExportFName = '/home/geve/Dropbox/EnKF_seir/run/IE.eps'
$!Export 
  ExportRegion = AllFrames


$!ActiveLineMaps -= [1-1606]
$!ActiveLineMaps += [101-300,901-1100]
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
