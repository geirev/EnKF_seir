#!MC 1410
$!OpenLayout  "./solutions.lay"
$!PrintSetup Palette = Color
$!ExportSetup ExportFormat = EPS
$!ExportSetup EPSPreviewImage{ImageType = None}


$!AlterData [1-18]
  Equation = 'V1=V1+43891'

# Total cases
$!ActiveLineMaps += [1-303]
$!LineMap [1-300]  Lines{Color = Custom44}
$!LineMap [1-300]  Assign{ShowInLegend = Never}
$!LineMap [1-300]  Lines{LineThickness = 0.40}

$!LineMap [301]  Assign{YAxisVar = 2}
$!LineMap [301]  Name = 'Hospitalized R(t)=0.85'
$!LineMap [301]  Lines{LineThickness = 0.80}
$!LineMap [301]  Lines{LinePattern = Dashed}
$!LineMap [301]  Lines{Color = Custom28}
$!LineMap [301]  Assign{ShowInLegend = Auto}

$!LineMap [302]   Name = 'Hospitalized R(t)=1.00'
$!LineMap [302]  Lines{LineThickness = 0.80}
$!LineMap [302]  Lines{LinePattern = LongDash}
$!LineMap [302]  Lines{Color = Custom28}
$!LineMap [302]  Assign{ShowInLegend = Auto}

$!LineMap [303]   Name = 'Hospitalized R(t)=1.15'
$!LineMap [303]  Lines{LineThickness = 0.80}
$!LineMap [303]  Lines{LinePattern = Solid}
$!LineMap [303]  Lines{Color = Custom28}
$!LineMap [303]  Assign{ShowInLegend = Auto}

# Total deaths
$!ActiveLineMaps += [304-603]
$!LineMap [304-403]  Lines{Color = Custom41}
$!LineMap [404-503]  Lines{Color = Custom41}
$!LineMap [504-603]  Lines{Color = Custom41}
$!LineMap [304-603]  Assign{ShowInLegend = Never}
$!LineMap [304-603]  Lines{LineThickness = 0.10}

$!ActiveLineMaps += [604]
$!LineMap [604]  Assign{YAxisVar = 2}
$!LineMap [604]  Name = 'Dead R(t)=0.85'
$!LineMap [604]  Lines{LineThickness = 0.80}
$!LineMap [604]  Lines{LinePattern = Dashed}
$!LineMap [604]  Lines{Color = Custom25}
$!LineMap [604]  Assign{ShowInLegend = Auto}

$!ActiveLineMaps += [605]
$!LineMap [605]   Name = 'Dead R(t)=1.00'
$!LineMap [605]  Lines{LineThickness = 0.80}
$!LineMap [605]  Lines{LinePattern = LongDash}
$!LineMap [605]  Lines{Color = Custom25}
$!LineMap [605]  Assign{ShowInLegend = Auto}

$!ActiveLineMaps += [606]
$!LineMap [606]   Name = 'Dead R(t)=1.15'
$!LineMap [606]  Lines{LineThickness = 0.80}
$!LineMap [606]  Lines{LinePattern = Solid}
$!LineMap [606]  Lines{Color = Custom25}
$!LineMap [606]  Assign{ShowInLegend = Auto}

$!ActiveLineMaps += [607]
$!ActiveLineMaps += [608]
$!LineMap [607]  Symbols{Size = 0.7 }
$!LineMap [608]  Symbols{Size = 0.7 }



$!GlobalLinePlot Legend{AnchorAlignment = TopLeft}
$!GlobalLinePlot Legend{XYPos{X = 9.19724}}
$!GlobalLinePlot Legend{XYPos{Y = 96.6097}}


$!XYLineAxis YDetail 1 {CoordScale = Linear}
$!XYLineAxis YDetail 1 {RangeMin = 0}
$!XYLineAxis YDetail 1 {RangeMax = 52000}
$!XYLineAxis XDetail 1 {RangeMin = 43891}
$!XYLineAxis XDetail 1 {RangeMax = 44142}
$!XYLineAxis YDetail 1 {Title{Offset = 7}}

$!RedrawAll 
$!ExportSetup ExportFName = './France_scenarioHD.eps'
$!Export 
  ExportRegion = AllFrames

