#!MC 1410
$!OpenLayout  "./solutions2.lay"
$!PrintSetup Palette = Color
$!ExportSetup ExportFormat = EPS
$!ExportSetup EPSPreviewImage{ImageType = None}


$!AlterData
  Equation = 'V1=V1+43876'

# Total cases
$!ActiveLineMaps += [1-303]
$!LineMap [1-300]  Lines{Color = Custom44}
$!LineMap [1-300]  Assign{ShowInLegend = Never}
$!LineMap [1-300]  Lines{LineThickness = 0.40}

$!LineMap [301]  Assign{YAxisVar = 2}
$!LineMap [301]  Name = 'Hospitalized R_3=0.75'
$!LineMap [301]  Lines{LineThickness = 0.80}
$!LineMap [301]  Lines{LinePattern = Dashed}
$!LineMap [301]  Lines{Color = Custom28}
$!LineMap [301]  Assign{ShowInLegend = Auto}

$!LineMap [302]   Name = 'Hospitalized R_3=0.85'
$!LineMap [302]  Lines{LineThickness = 0.80}
$!LineMap [302]  Lines{LinePattern = LongDash}
$!LineMap [302]  Lines{Color = Custom28}
$!LineMap [302]  Assign{ShowInLegend = Auto}

$!LineMap [303]   Name = 'Hospitalized R_3=1.00'
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
$!LineMap [604]  Name = 'Dead R_3=0.75'
$!LineMap [604]  Lines{LineThickness = 0.80}
$!LineMap [604]  Lines{LinePattern = Dashed}
$!LineMap [604]  Lines{Color = Custom25}
$!LineMap [604]  Assign{ShowInLegend = Auto}

$!ActiveLineMaps += [605]
$!LineMap [605]   Name = 'Dead R_3=0.85'
$!LineMap [605]  Lines{LineThickness = 0.80}
$!LineMap [605]  Lines{LinePattern = LongDash}
$!LineMap [605]  Lines{Color = Custom25}
$!LineMap [605]  Assign{ShowInLegend = Auto}

$!ActiveLineMaps += [606]
$!LineMap [606]   Name = 'Dead R_3=1.00'
$!LineMap [606]  Lines{LineThickness = 0.80}
$!LineMap [606]  Lines{LinePattern = Solid}
$!LineMap [606]  Lines{Color = Custom25}
$!LineMap [606]  Assign{ShowInLegend = Auto}

$!ActiveLineMaps += [607]
$!ActiveLineMaps += [608]
$!LineMap [607]  Symbols{Size = 0.7 }
$!LineMap [608]  Symbols{Size = 0.7 }



$!GlobalLinePlot Legend{AnchorAlignment = TopRight}
$!GlobalLinePlot Legend{XYPos{X = 98.2065}}
$!GlobalLinePlot Legend{XYPos{Y = 96.6097}}

#$!XYLineAxis ViewportPosition{X1 = 10.5}
$!XYLineAxis YDetail 1 {Title{Offset = 7.00}}


$!XYLineAxis YDetail 1 {CoordScale = Linear}
$!XYLineAxis YDetail 1 {RangeMin = 0}
$!XYLineAxis YDetail 1 {RangeMax = 40000}
$!XYLineAxis XDetail 1 {RangeMin = 43890}
$!XYLineAxis XDetail 1 {RangeMax = 44056}

$!XYLineAxis YDetail 1 {Title{TextShape{Height = 5.6}}}
$!XYLineAxis XDetail 1 {Title{TextShape{Height = 5.6}}}
$!XYLineAxis XDetail 1 {TickLabel{TextShape{Height = 5}}}
$!XYLineAxis YDetail 1 {TickLabel{TextShape{Height = 5}}}
$!XYLineAxis YDetail 1 {TickLabel{Angle = 45}}
$!GlobalLinePlot Legend{TextShape{Height = 5}}
$!GlobalLinePlot Legend{Box{Margin = 2}}

$!RedrawAll 
$!ExportSetup ExportFName = './France_scenarioHD.eps'
$!Export 
  ExportRegion = AllFrames

