#!MC 1410
$!OpenLayout  "./solutions.lay"
$!PrintSetup Palette = Color
$!ExportSetup ExportFormat = EPS
$!ExportSetup EPSPreviewImage{ImageType = None}


$!AlterData [1-18]
  Equation = 'V1=V1+43900'

$!ActiveLineMaps -= [1-910]

# Total deaths
$!LineMap [304-403]  Lines{Color = Custom44}
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
$!LineMap [604]  Lines{Color = Custom28}
$!LineMap [604]  Assign{ShowInLegend = Auto}

$!LineMap [606]  Name = 'Deaths 5 % measurement error'
$!LineMap [606]  Lines{LineThickness = 0.80}
$!LineMap [606]  Lines{LinePattern = Solid}
$!LineMap [606]  Lines{Color = Custom25}
$!LineMap [606]  Assign{ShowInLegend = Auto}

$!LineMap [607]  Symbols{Size = 0.7}
$!LineMap [607]  Name = 'Observed deaths'
$!LineMap [607]  Assign{ShowInLegend = Auto}



$!GlobalLinePlot Legend{AnchorAlignment = TopLeft}
$!GlobalLinePlot Legend{XYPos{X = 9.19724}}
$!GlobalLinePlot Legend{XYPos{Y = 96.6097}}


$!XYLineAxis YDetail 1 {CoordScale = Linear}
$!XYLineAxis YDetail 1 {RangeMin = 0}
$!XYLineAxis YDetail 1 {RangeMax = 100000}
$!XYLineAxis XDetail 1 {RangeMin = 43900}
$!XYLineAxis XDetail 1 {RangeMax = 44220}
$!XYLineAxis YDetail 1 {Title{Offset = 7}}

$!RedrawAll 
$!ExportSetup ExportFName = './Brazil_scenarioCD.eps'
$!Export 
  ExportRegion = AllFrames
