format 221

classinstance 128021 class_ref 128405 // Controller
 name ""  xyz 374 232 2000
classinstance 128149 class_ref 129045 // ConfigurationView
 name ""  xyz 290 364 2000
classinstancecanvas 128277 classinstance_ref 128021 // User
  xyz 104 364 2000
end
classinstance 128661 class_ref 129941 // TraceContext
 name ""  xyz 628 229 2000
note 129813 "The user can add/remove a severity filter on a name:LoggerName"
  xyzwh 80 113 2000 231 83
linkcanvas 128405
  from ref 128277 z 2001 to ref 128149
dirscanvas 129045 z 1000 linkcanvas_ref 128405
  
  forward_label "1 addSeverityFilter()
4 removeSeverityFilter()" xyz 160 328 3000
linkcanvas 128533
  from ref 128149 z 2001 to ref 128021
dirscanvas 128917 z 1000 linkcanvas_ref 128533
  
  forward_label "2 addSeverityFilter()
5 removeSeverityFilter()" xyz 239 266 3000
linkcanvas 128789
  from ref 128021 z 2001 to ref 128661
dirscanvas 129429 z 1000 linkcanvas_ref 128789
  
  forward_label "3 storeSeverityFilter()
6 removeSeverityFilter()" xyz 464 194 3000
msgs
  msg operation_ref 128277 // "addSeverityFilter()"
    forward ranks 1 "1" dirscanvas_ref 129045
    msgs
      msg operation_ref 128405 // "addSeverityFilter()"
	forward ranks 2 "1.1" dirscanvas_ref 128917
	msgs
	  msg operation_ref 128149 // "storeSeverityFilter()"
	    forward ranks 3 "1.1.1" dirscanvas_ref 129429
	    no_msg
	msgsend
    msgsend
  msg operation_ref 128533 // "removeSeverityFilter()"
    forward ranks 4 "2" dirscanvas_ref 129045
    msgs
      msg operation_ref 128661 // "removeSeverityFilter()"
	forward ranks 5 "2.1" dirscanvas_ref 128917
	msgs
	  msg operation_ref 128021 // "removeSeverityFilter()"
	    forward ranks 6 "2.1.1" dirscanvas_ref 129429
	    no_msg
	msgsend
    msgsend
msgsend
end
