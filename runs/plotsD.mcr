#!MC 1410
$!OpenLayout  "./solutions.lay"
$!PrintSetup Palette = Color
$!ExportSetup ExportFormat = EPS
$!ExportSetup EPSPreviewImage{ImageType = None}

$!FrameControl ActivateByNumber
  Frame = 1
$!LineMap [1-1701]  Lines{LineThickness = 1.5}
$!LineMap [100,200,300,400,500,600,700,800,900,1000]  Lines{LineThickness = 0.400000000000000044}
$!LineMap [1100,1200,1300,1400,1500,1600,1700,1701]  Lines{LineThickness = 0.400000000000000044}
$!LineMap [100,200,300,400,500,600,700,800,900,1000]  Assign{YAxisVar = 2}
$!LineMap [1100,1200,1300,1400,1500,1600,1700,1701]  Assign{YAxisVar = 2}




$!FrameControl ActivateByNumber
  Frame = 1

$!GlobalLinePlot Legend{AnchorAlignment = TopLeft}
$!GlobalLinePlot Legend{XYPos{X = 10.0}}
$!GlobalLinePlot Legend{XYPos{Y = 95.0}}

$!AlterData [1-18]
  Equation = 'V1=V1+XXXXA'

$!FrameControl ActivateByNumber
  Frame = 2
$!AlterData [1-3]
  Equation = 'V2=V2+XXXXA'





###############################
$!FrameControl ActivateByNumber
  Frame = 1
$!ActiveLineMaps -= [1-1701]
$!ActiveLineMaps += [801-900]
$!ActiveLineMaps += [1201-1300]
$!ActiveLineMaps += [1301-1400]
$!ActiveLineMaps += [1401-1500]

$!FrameControl ActivateByNumber
  Frame = 2
$!ActiveLineMaps += [1-3]

$!XYLineAxis YDetail 1 {CoordScale = Log}
$!XYLineAxis YDetail 1 {RangeMin = 10}
$!XYLineAxis YDetail 1 {RangeMax = YYYYC}
$!XYLineAxis XDetail 1 {RangeMin = XXXXA}
$!XYLineAxis XDetail 1 {RangeMax = XXXXB}

$!RedrawAll 
$!ExportSetup ExportFName = './HDlog.eps'
$!Export 
  ExportRegion = AllFrames
$!PAUSE "HDlog"




###############################
$!FrameControl ActivateByNumber
  Frame = 1
$!ActiveLineMaps -= [1-1701]
$!ActiveLineMaps += [1201-1300]
$!ActiveLineMaps += [1301-1400]

$!FrameControl ActivateByNumber
  Frame = 2
$!ActiveLineMaps += [1-2]
$!ActiveLineMaps -= [3]

$!XYLineAxis YDetail 1 {CoordScale = Linear}
$!XYLineAxis YDetail 1 {RangeMin = 0}
$!XYLineAxis YDetail 1 {RangeMax = YYYYD}
$!XYLineAxis XDetail 1 {RangeMin = XXXXA}
$!XYLineAxis XDetail 1 {RangeMax = XXXXB}
 
$!RedrawAll 
$!ExportSetup ExportFName = './HD.eps'
$!Export 
  ExportRegion = AllFrames
$!PAUSE "HD"


###############################
#$!FrameControl ActivateByNumber
#  Frame = 1
#$!ActiveLineMaps -= [1-1701]
#$!ActiveLineMaps += [1001-1100]
#$!ActiveLineMaps += [1101-1200]
#$!FrameControl ActivateByNumber
#  Frame = 2
#$!ActiveLineMaps -= [1-3]
#
#$!XYLineAxis YDetail 1 {CoordScale = Linear}
#$!XYLineAxis YDetail 1 {RangeMin = 0}
#$!XYLineAxis YDetail 1 {RangeMax = YYYYI}
#$!XYLineAxis XDetail 1 {RangeMin = XXXXA}
#$!XYLineAxis XDetail 1 {RangeMax = XXXXB}
#
#$!RedrawAll 
#$!ExportSetup ExportFName = './IE.eps'
#$!Export 
#  ExportRegion = AllFrames
#$!PAUSE "IE"
#
#
################################
#$!FrameControl ActivateByNumber
#  Frame = 1
#$!ActiveLineMaps -= [1-1701]
#$!ActiveLineMaps += [801-900]
#$!ActiveLineMaps += [901-1000]
#
#$!FrameControl ActivateByNumber
#  Frame = 2
#$!ActiveLineMaps -= [1-2]
#$!ActiveLineMaps += [3]
#
#$!XYLineAxis YDetail 1 {CoordScale = Linear}
#$!XYLineAxis YDetail 1 {RangeMin = 0}
#$!XYLineAxis YDetail 1 {RangeMax = YYYYC}
#$!XYLineAxis XDetail 1 {RangeMin = XXXXA}
#$!XYLineAxis XDetail 1 {RangeMax = XXXXB}
#$!RedrawAll 
#$!ExportSetup ExportFName = './CR.eps'
#$!Export 
#  ExportRegion = AllFrames
#$!PAUSE "CR"
#

###############################
# R E N S
$!FrameControl ActivateByNumber
  Frame = 1
$!ActiveLineMaps -= [1-1701]
$!ActiveLineMaps += [1501-1600]
$!ActiveLineMaps += [1601-1701]
$!View Fit

$!GlobalLinePlot Legend{AnchorAlignment = TopLeft}
$!GlobalLinePlot Legend{XYPos{X = 70.0}}
$!GlobalLinePlot Legend{XYPos{Y = 95.0}}

$!FrameControl ActivateByNumber
  Frame = 2
$!ActiveLineMaps -= [1-3]
$!XYLineAxis YDetail 1 {CoordScale = Linear}
$!XYLineAxis YDetail 1 {RangeMin = 0}
$!XYLineAxis XDetail 1 {RangeMin = XXXXA}
$!XYLineAxis XDetail 1 {RangeMax = XXXXB}


$!AttachGeom
  AnchorPos
    {
    X = 43800.0
    Y = 1.0
    }
  Color = Red
  LineThickness = 0.2
  RawData
1
2
0 0
480.000 0.0

$!RedrawAll 
$!ExportSetup ExportFName = './RENS.eps'
$!Export 
  ExportRegion = AllFrames
$!PAUSE "RENS"
