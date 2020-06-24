#!MC 1410
$!LOOP 3
$!OpenLayout  "./solutions|LOOP|.lay"
$!PrintSetup Palette = Color
$!ExportSetup ExportFormat = EPS
$!ExportSetup EPSPreviewImage{ImageType = None}


$!AlterData [1-18]
  Equation = 'V1=V1+43899'

$!ActiveLineMaps -= [1-910]

# Total deaths
$!LineMap [304-403]  Lines{Color = Custom42}
$!LineMap [504-603]  Lines{Color = Custom41}
$!LineMap [304-603]  Assign{ShowInLegend = Never}
$!LineMap [304-603]  Lines{LineThickness = 0.40}

$!ActiveLineMaps += [304-403]  # Case01
$!ActiveLineMaps += [504-603]  # Case02
$!ActiveLineMaps += [604]      # Case01 mean
$!ActiveLineMaps += [606]      # Case03 mean
$!ActiveLineMaps += [607]      # Death 

$!LineMap [604]  Assign{YAxisVar = 2}
$!LineMap [604]  Name = 'Deaths 20 % measurement error'
$!LineMap [604]  Lines{LineThickness = 0.80}
$!LineMap [604]  Lines{LinePattern = Solid}
$!LineMap [604]  Lines{Color = Custom26}
$!LineMap [604]  Assign{ShowInLegend = Auto}

$!LineMap [606]  Name = 'Deaths 5 % measurement error'
$!LineMap [606]  Lines{LineThickness = 0.80}
$!LineMap [606]  Lines{LinePattern = Solid}
$!LineMap [606]  Lines{Color = Custom25}
$!LineMap [606]  Assign{ShowInLegend = Auto}

$!LineMap [607]  Symbols{Size = 0.7}
$!LineMap [607]  Name = 'Observed deaths'
$!LineMap [607]  Assign{ShowInLegend = Auto}





$!XYLineAxis YDetail 1 {CoordScale = Linear}
$!XYLineAxis YDetail 1 {RangeMin = 0}
$!XYLineAxis YDetail 1 {RangeMax = 50000}
$!XYLineAxis XDetail 1 {RangeMin = 43899}
$!XYLineAxis XDetail 1 {RangeMax = 44220}
$!XYLineAxis YDetail 1 {Title{Offset = 7}}

$!XYLineAxis YDetail 1 {Title{TextShape{Height = 5.6}}}
$!XYLineAxis XDetail 1 {Title{TextShape{Height = 5.6}}}
$!XYLineAxis XDetail 1 {TickLabel{TextShape{Height = 5}}}
$!XYLineAxis YDetail 1 {TickLabel{TextShape{Height = 5}}}
$!XYLineAxis YDetail 1 {Title{Offset = 8}}
$!XYLineAxis ViewportPosition{X1 = 10.2}
$!GlobalLinePlot Legend{TextShape{Height = 5}}
$!GlobalLinePlot Legend{Box{Margin = 3}}
$!GlobalLinePlot Legend{AnchorAlignment = TopLeft}
$!GlobalLinePlot Legend{XYPos{X = 10.2757}}
$!GlobalLinePlot Legend{XYPos{Y = 96.5333}}

$!RedrawAll 
$!ExportSetup ExportFName = './Brazil_scenarioCD|LOOP|.eps'
$!Export 
  ExportRegion = AllFrames


#
################################
## R E N S
$!OpenLayout  "./rens|LOOP|.lay"

#$!AlterData [1-2]
#  Equation = 'V1=V1+43899'

$!ActiveLineMaps += [1-202]

$!LineMap [1-200]  Assign{ShowInLegend = Never}
$!XYLineAxis YDetail 1 {Title{Text = 'Effective R(t)'}}
#
$!XYLineAxis YDetail 1 {CoordScale = Linear}
$!View Fit
$!XYLineAxis XDetail 1 {RangeMin = 43899}
$!XYLineAxis XDetail 1 {RangeMax = 44220}
$!XYLineAxis YDetail 1 {RangeMin = 0}
$!XYLineAxis YDetail 1 {RangeMax = 5}
$!XYLineAxis YDetail 1 {GRSpacing = 1}
#
#
$!GlobalLinePlot Legend{AnchorAlignment = TopRight}
$!GlobalLinePlot Legend{XYPos{X = 98.1616}}
$!GlobalLinePlot Legend{XYPos{Y = 96.5333}}

$!XYLineAxis YDetail 1 {Title{TextShape{Height = 5.6}}}
$!XYLineAxis XDetail 1 {Title{TextShape{Height = 5.6}}}
$!XYLineAxis XDetail 1 {TickLabel{TextShape{Height = 5}}}
$!XYLineAxis YDetail 1 {TickLabel{TextShape{Height = 5}}}
$!XYLineAxis YDetail 1 {Title{Offset = 8}}
$!GlobalLinePlot Legend{TextShape{Height = 5}}
#
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
$!ExportSetup ExportFName = './Brazil_scenarioRENS|LOOP|.eps'
$!Export 
  ExportRegion = AllFrames


$!ENDLOOP
