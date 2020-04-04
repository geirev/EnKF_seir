set noautoindent 
set noshowmatch 
set autowrite 
set wrapmargin=8 
set report=1 
set tabstop=50 
set shiftwidth=3 
set nomagic
map  :set ts=4
map  :!pdflatex %
map  o\lhead{\scriptsize\tt:r!pwd; print "/%}"kkJJx
map  :!evince $(basename % \.tex).pdf 2> /dev/null & 
map [6~ 
map [5~ 
map [A  k
map [D  h
map [C  l
map [B  j
map! [A  ka
map! [D  ha
map! [C  la
map! [B  ja
map! å {\aa}
map! Å {\AA}
map! æ {\ae}
map! Æ {\AE}
map! ø {\o}
map! Ø {\O}
map! ° {}i
:ab ba %\begin{array}{lcr  }  &  &  &  \\\end{array}%kkI
:ab bart \documentstyle[equations,newnice,mywide,12pt]{article}\input{def}\begin{document}\end{document}
:ab bc \begin{center}\end{center}k0i
:ab bd %\begin{description}\item[]\end{description}%kk$i
:ab ben %\begin{enumerate}\item\end{enumerate}%kkA
:ab beq %\begin{equation}\label{eq:}\end{equation}%0kkki
:ab balign %\begin{align} &  \label{eq:}\\   & \label{eq:}\end{align}%kkki
:ab bsplit \begin{split}  & \\  &  \end{split}
:ab bfi %\begin{figure}[tbhp]\caption{   \label{fig:}}\end{figure}%kkkI
:ab bi %\begin{itemize}\item\end{itemize}%kkA 
:ab bq %\begin{quote}\end{quote}%kkI
:ab bt %\begin{table}[tb]\caption{\label{tab:}}\end{table}%kkk0i
:ab btab \begin{tabular}{}\hline &  &\\\hline\end{tabular}
:ab btable bt bc btab
:ab btb %\begin{tabbing} \= \= \=   \kill\>     \\\end{tabbing}%%kkkI
:ab bv %\begin{verbatim}\end{verbatim}%kki
:ab bref (\ref{eq:})F:
:ab brefe Eq.~(\ref{eq:})F:
:ab brefee Eqs.~(\ref{eq:}) and (\ref{eq:})2F:
:ab brefs Sec.~\ref{sec:}F:
:ab breft Tab.~\ref{tab:}F:
:ab breff Fig.~\ref{fig:}F:
:ab brefff Figs.~\ref{fig:} and \ref{fig:}F:
:ab bind \setlength{\parindent}{0pt} \setlength{\parskip}{3mm}
:ab bfra \frac{}{}2F{
