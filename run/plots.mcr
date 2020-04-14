#!MC 1410
$!OpenLayout  "/home/geve/Dropbox/EnKF_seir/run/seir.lay"
$!ExportSetup ExportFormat = EPS
$!ExportSetup ImageWidth = 1559
$!ExportSetup EPSPreviewImage{ImageType = None}
$!ExportSetup ExportFName = '/home/geve/Dropbox/EnKF_seir/run/HD.eps'
$!Export 
  ExportRegion = AllFrames

$!XYLineAxis YDetail 1 {RangeMin = 0}
$!XYLineAxis XDetail 1 {RangeMin = 0}
$!XYLineAxis XDetail 1 {RangeMax = 100}


$!ActiveLineMaps -= [1-1606]
$!ActiveLineMaps += [301-500,1101-1300]
$!XYLineAxis YDetail 1 {RangeMax = 60000}
$!ExportSetup ExportFName = '/home/geve/Dropbox/EnKF_seir/run/IE.eps'
$!Export 
  ExportRegion = AllFrames


$!ActiveLineMaps -= [1-1606]
$!ActiveLineMaps += [301-500,1101-1300]
$!XYLineAxis YDetail 1 {RangeMax = 60000}
$!ExportSetup ExportFName = '/home/geve/Dropbox/EnKF_seir/run/IE.eps'
$!Export 
  ExportRegion = AllFrames



$!ActiveLineMaps -= [1-1606]
$!ActiveLineMaps += [101-300,901-1100]
$!XYLineAxis YDetail 1 {RangeMax = 100000}
$!ExportSetup ExportFName = '/home/geve/Dropbox/EnKF_seir/run/CR.eps'
$!Export 
  ExportRegion = AllFrames
