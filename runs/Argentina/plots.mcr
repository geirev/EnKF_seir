#!MC 1410
$!OpenLayout  "./solutions.lay"
$!PrintSetup Palette = Color
$!ExportSetup ExportFormat = EPS
$!ExportSetup EPSPreviewImage{ImageType = None}


$!AlterData [1-18]
  Equation = 'V1=V1+43891'

# Total cases
$!ActiveLineMaps += [1-303]
$!LineMap [1-300]  Lines{Color = Custom43}
$!LineMap [1-300]  Assign{ShowInLegend = Never}
$!LineMap [1-300]  Lines{LineThickness = 0.40}

$!LineMap [301]  Assign{YAxisVar = 2}
$!LineMap [301]  Name = 'Cases R(t)=1.4'
$!LineMap [301]  Lines{LineThickness = 0.80}
$!LineMap [301]  Lines{LinePattern = Dashed}
$!LineMap [301]  Lines{Color = Custom27}
$!LineMap [301]  Assign{ShowInLegend = Auto}

$!LineMap [302]   Name = 'Cases R(t)=1.0'
$!LineMap [302]  Lines{LineThickness = 0.80}
$!LineMap [302]  Lines{LinePattern = LongDash}
$!LineMap [302]  Lines{Color = Custom27}
$!LineMap [302]  Assign{ShowInLegend = Auto}

$!LineMap [303]   Name = 'Cases R(t)=0.8'
$!LineMap [303]  Lines{LineThickness = 0.80}
$!LineMap [303]  Lines{LinePattern = Solid}
$!LineMap [303]  Lines{Color = Custom27}
$!LineMap [303]  Assign{ShowInLegend = Auto}

# Total deaths
$!ActiveLineMaps += [304-607]
$!LineMap [304-603]  Lines{Color = Custom41}
$!LineMap [304-603]  Assign{ShowInLegend = Never}
$!LineMap [304-603]  Lines{LineThickness = 0.40}

$!LineMap [604]  Assign{YAxisVar = 2}
$!LineMap [604]  Name = 'Dead R(t)=1.4'
$!LineMap [604]  Lines{LineThickness = 0.80}
$!LineMap [604]  Lines{LinePattern = Dashed}
$!LineMap [604]  Lines{Color = Custom25}
$!LineMap [604]  Assign{ShowInLegend = Auto}

$!LineMap [605]   Name = 'Dead R(t)=1.0'
$!LineMap [605]  Lines{LineThickness = 0.80}
$!LineMap [605]  Lines{LinePattern = LongDash}
$!LineMap [605]  Lines{Color = Custom25}
$!LineMap [605]  Assign{ShowInLegend = Auto}

$!LineMap [606]   Name = 'Dead R(t)=0.8'
$!LineMap [606]  Lines{LineThickness = 0.80}
$!LineMap [606]  Lines{LinePattern = Solid}
$!LineMap [606]  Lines{Color = Custom25}
$!LineMap [606]  Assign{ShowInLegend = Auto}

$!LineMap [607]  Symbols{Size = 0.7 }



$!GlobalLinePlot Legend{AnchorAlignment = TopLeft}
$!GlobalLinePlot Legend{XYPos{X = 9.19724}}
$!GlobalLinePlot Legend{XYPos{Y = 96.6097}}


$!XYLineAxis YDetail 1 {CoordScale = Log}
$!XYLineAxis YDetail 1 {RangeMin = 10}
$!XYLineAxis YDetail 1 {RangeMax = 1000000}
$!XYLineAxis XDetail 1 {RangeMin = 43891}
$!XYLineAxis XDetail 1 {RangeMax = 44050}

$!RedrawAll 
$!ExportSetup ExportFName = './Argentina_scenarioCD.eps'
$!Export 
  ExportRegion = AllFrames

#
################################
## R E N S
$!ActiveLineMaps -= [1-607]
$!ActiveLineMaps += [608-910]
$!XYLineAxis YDetail 1 {Title{Text = 'Effective R(t)'}}
#
$!LineMap [908]  Name = 'Posterior R(t)=A'
$!LineMap [909]  Name = 'Posterior R(t)=B'
$!LineMap [910]  Name = 'Posterior R(t)=C'
$!LineMap [908]  Assign{ShowInLegend = Auto}
$!LineMap [909]  Assign{ShowInLegend = Auto}
$!LineMap [910]  Assign{ShowInLegend = Auto}
$!LineMap [908-910]  Lines{LineThickness = 0.80}
#
$!XYLineAxis YDetail 1 {CoordScale = Linear}
$!View Fit
$!XYLineAxis XDetail 1 {RangeMin = 43891}
$!XYLineAxis XDetail 1 {RangeMax = 44050}
$!XYLineAxis YDetail 1 {RangeMin = 0}
$!XYLineAxis YDetail 1 {RangeMax = 5}
$!XYLineAxis YDetail 1 {GRSpacing = 1}
#
#
$!GlobalLinePlot Legend{AnchorAlignment = TopLeft}
$!GlobalLinePlot Legend{XYPos{X = 70.0}}
$!GlobalLinePlot Legend{XYPos{Y = 95.0}}
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
$!ExportSetup ExportFName = './Argentina_scenarioRENS.eps'
$!Export 
  ExportRegion = AllFrames
