#!MC 1410
$!LOOP 3
$!OpenLayout  "./solutions|LOOP|.lay"
$!PrintSetup Palette = Color
$!ExportSetup ExportFormat = EPS
$!ExportSetup EPSPreviewImage{ImageType = None}

$!AlterData [1-15]
  Equation = 'V1=V1+43880'

# Active cases
$!ActiveLineMaps += [1-303]
$!LineMap [1-300]  Lines{Color = Custom48}
$!LineMap [1-300]  Assign{ShowInLegend = Never}
$!LineMap [1-300]  Lines{LineThickness = 0.40}

$!LineMap [301]  Assign{YAxisVar = 2}
$!LineMap [301]  Name = 'Active Cases R(t)=0.5'
$!LineMap [301]  Lines{LineThickness = 0.80}
$!LineMap [301]  Lines{LinePattern = DashDot}
$!LineMap [301]  Lines{Color = Custom32}
$!LineMap [301]  Assign{ShowInLegend = Never}

$!LineMap [302]   Name = 'Active Cases R(t)=1.0'
$!LineMap [302]  Lines{LineThickness = 0.80}
$!LineMap [302]  Lines{LinePattern = LongDash}
$!LineMap [302]  Lines{Color = Custom32}
$!LineMap [302]  Assign{ShowInLegend = Never}

$!LineMap [303]   Name = 'Active Cases R(t)=1.2'
$!LineMap [303]  Name = 'Active Cases'
$!LineMap [303]  Lines{LineThickness = 0.80}
$!LineMap [303]  Lines{LinePattern = Solid}
$!LineMap [303]  Lines{Color = Custom32}
$!LineMap [303]  Assign{ShowInLegend = Auto}

# Total Cases
$!ActiveLineMaps += [305-607]
$!LineMap [305-604]  Lines{Color = Custom43}
$!LineMap [305-604]  Assign{ShowInLegend = Never}
$!LineMap [305-604]  Lines{LineThickness = 0.40}

$!LineMap [605]  Assign{YAxisVar = 2}
$!LineMap [605]  Name = 'Cases R(t)=0.5'
$!LineMap [605]  Lines{LineThickness = 0.80}
$!LineMap [605]  Lines{LinePattern = DashDot}
$!LineMap [605]  Lines{Color = Custom27}
$!LineMap [605]  Assign{ShowInLegend = Never}

$!LineMap [606]   Name = 'Cases R(t)=1.0'
$!LineMap [606]  Lines{LineThickness = 0.80}
$!LineMap [606]  Lines{LinePattern = LongDash}
$!LineMap [606]  Lines{Color = Custom27}
$!LineMap [606]  Assign{ShowInLegend = Never}

$!LineMap [607]   Name = 'Cases R(t)=1.2'
$!LineMap [607]  Name = 'Total Cases'
$!LineMap [607]  Lines{LineThickness = 0.80}
$!LineMap [607]  Lines{LinePattern = Solid}
$!LineMap [607]  Lines{Color = Custom27}
$!LineMap [607]  Assign{ShowInLegend = Auto}

$!IF |LOOP| > 2
$!ActiveLineMaps += [608]
$!ENDIF


# Hospitalized
$!ActiveLineMaps += [609-911]
$!LineMap [609-908]  Lines{Color = Custom44}
$!LineMap [609-908]  Assign{ShowInLegend = Never}
$!LineMap [609-908]  Lines{LineThickness = 0.40}

$!LineMap [909]  Assign{YAxisVar = 2}
$!LineMap [909]  Name = 'Cases R(t)=0.5'
$!LineMap [909]  Lines{LineThickness = 0.80}
$!LineMap [909]  Lines{LinePattern = DashDot}
$!LineMap [909]  Lines{Color = Custom36}
$!LineMap [909]  Assign{ShowInLegend = Never}

$!LineMap [910]   Name = 'Cases R(t)=1.0'
$!LineMap [910]  Lines{LineThickness = 0.80}
$!LineMap [910]  Lines{LinePattern = LongDash}
$!LineMap [910]  Lines{Color = Custom36}
$!LineMap [910]  Assign{ShowInLegend = Never}

$!LineMap [911]   Name = 'Hospitalized R(t)=1.2'
$!LineMap [911]   Name = 'Hospitalized'
$!LineMap [911]  Lines{LineThickness = 0.80}
$!LineMap [911]  Lines{LinePattern = Solid}
$!LineMap [911]  Lines{Color = Custom36}
$!LineMap [911]  Assign{ShowInLegend = Auto}

$!IF |LOOP| > 1
$!ActiveLineMaps += [912]
$!ENDIF

# Total deaths
$!ActiveLineMaps += [913-1215]
$!LineMap [913-1212]  Lines{Color = Custom41}
$!LineMap [913-1212]  Assign{ShowInLegend = Never}
$!LineMap [913-1212]  Lines{LineThickness = 0.40}

$!LineMap [1213]  Assign{YAxisVar = 2}
$!LineMap [1213]  Name = 'Dead R(t)=0.5'
$!LineMap [1213]  Lines{LineThickness = 0.80}
$!LineMap [1213]  Lines{LinePattern = DashDot}
$!LineMap [1213]  Lines{Color = Custom25}
$!LineMap [1213]  Assign{ShowInLegend = Never}

$!LineMap [1214]   Name = 'Dead R(t)=1.0'
$!LineMap [1214]  Lines{LineThickness = 0.80}
$!LineMap [1214]  Lines{LinePattern = LongDash}
$!LineMap [1214]  Lines{Color = Custom25}
$!LineMap [1214]  Assign{ShowInLegend = Never}

$!LineMap [1215]   Name = 'Dead R(t)=1.2'
$!LineMap [1215]   Name = 'Total Dead'
$!LineMap [1215]  Lines{LineThickness = 0.80}
$!LineMap [1215]  Lines{LinePattern = Solid}
$!LineMap [1215]  Lines{Color = Custom25}
$!LineMap [1215]  Assign{ShowInLegend = Auto}




$!GlobalLinePlot Legend{AnchorAlignment = BottomLeft}
$!GlobalLinePlot Legend{XYPos{X = 24.1103}}
$!GlobalLinePlot Legend{XYPos{Y = 10.3651}}


$!XYLineAxis YDetail 1 {CoordScale = Log}
$!XYLineAxis YDetail 1 {RangeMin = 10}
$!XYLineAxis YDetail 1 {RangeMax = 20000000}
$!XYLineAxis XDetail 1 {RangeMin = 43891}
$!XYLineAxis XDetail 1 {RangeMax = 44070}

$!XYLineAxis YDetail 1 {Title{TextShape{Height = 5.6}}}
$!XYLineAxis XDetail 1 {Title{TextShape{Height = 5.6}}}
$!XYLineAxis XDetail 1 {TickLabel{TextShape{Height = 5}}}
$!XYLineAxis YDetail 1 {TickLabel{TextShape{Height = 5}}}
$!GlobalLinePlot Legend{TextShape{Height = 5}}

$!RedrawAll 
$!ExportSetup ExportFName = './England_scenario|LOOP|.eps'
$!Export 
  ExportRegion = AllFrames

$!ENDLOOP



#
################################
## R E N S
$!LOOP 3
$!OpenLayout  "./rens|LOOP|.lay"

$!AlterData [1-6]
  Equation = 'V1=V1+43880'

$!ActiveLineMaps += [1-609]
$!LineMap [1-609]  Assign{ShowInLegend = Never}
$!XYLineAxis YDetail 1 {Title{Text = 'Effective R(t)'}}
#
$!LineMap [604]  Name = 'R_3=0.5'
$!LineMap [604]  Lines{LinePattern = DashDot}
$!LineMap [604]  Assign{ShowInLegend = Auto}
$!LineMap [605]  Name = 'R_3=1.0'
$!LineMap [605]  Lines{LinePattern = LongDash}
$!LineMap [605]  Assign{ShowInLegend = Auto}
$!LineMap [606]  Name = 'R_3=1.2'
$!LineMap [606]  Lines{LinePattern = Solid}
$!LineMap [606]  Assign{ShowInLegend = Auto}
$!LineMap [604-606]  Lines{LineThickness = 0.80}
#
$!XYLineAxis YDetail 1 {CoordScale = Linear}
$!View Fit
$!XYLineAxis XDetail 1 {RangeMin = 43891}
$!XYLineAxis XDetail 1 {RangeMax = 44070}
$!XYLineAxis YDetail 1 {RangeMin = 0}
$!XYLineAxis YDetail 1 {RangeMax = 5}
$!XYLineAxis YDetail 1 {GRSpacing = 1}
#
#
$!GlobalLinePlot Legend{AnchorAlignment = TopRight}
$!GlobalLinePlot Legend{XYPos{X = 98.2363}}
$!GlobalLinePlot Legend{XYPos{Y = 96.6866}}

$!XYLineAxis YDetail 1 {Title{TextShape{Height = 5.6}}}
$!XYLineAxis XDetail 1 {Title{TextShape{Height = 5.6}}}
$!XYLineAxis XDetail 1 {TickLabel{TextShape{Height = 5}}}
$!XYLineAxis YDetail 1 {TickLabel{TextShape{Height = 5}}}
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
$!ExportSetup ExportFName = './England_scenarioRENS|LOOP|.eps'
$!Export 
  ExportRegion = AllFrames
$!ENDLOOP
