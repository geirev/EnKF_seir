#!MC 1410
$!LOOP 2

$!OpenLayout  "./solutions|LOOP|.lay"
$!PrintSetup Palette = Color
$!ExportSetup ExportFormat = EPS
$!ExportSetup EPSPreviewImage{ImageType = None}

$!LineMap [1-103]  Lines{LineThickness = 0.4}

# ('Quebec convolution routine for dead variable')
$!LOOP 103
$!IF |LOOP| > 1
$!AlterData[1]
  Equation = 'V|LOOP| = 0.37*V|LOOP|(i-8) + 0.32*V|LOOP|(i-4) + 0.31*V|LOOP|(i)'
$!ENDIF
$!ENDLOOP

$!AlterData [1-3]
  Equation = 'V1=V1+43897'

# Total dead
$!LineMap [101]  Name = 'Total dead'
$!LineMap [102]  Name = 'Observed deaths'
$!LineMap [103]  Name = 'Verification deaths'

$!GlobalLinePlot Legend{AnchorAlignment = TopLeft}
$!GlobalLinePlot Legend{XYPos{X = 9.19724}}
$!GlobalLinePlot Legend{XYPos{Y = 96.6097}}

$!XYLineAxis YDetail 1 {CoordScale = Linear}
$!XYLineAxis YDetail 1 {RangeMin = 0}
$!XYLineAxis YDetail 1 {RangeMax = 600}
$!XYLineAxis XDetail 1 {RangeMin = 43905}
$!XYLineAxis XDetail 1 {RangeMax = 43937}
$!XYLineAxis YDetail 1 {Title{Offset = 7}}
 
$!RedrawAll 
$!ExportSetup ExportFName = './QuebecScenario|LOOP|.eps'
$!Export 
  ExportRegion = AllFrames
$!ENDLOOP 
