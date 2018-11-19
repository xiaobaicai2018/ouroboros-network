format 221

classinstancecanvas 128021 classinstance_ref 128021 // User
  xyz 136.8 263.6 2000
end
classinstance 128277 class_ref 128021 // Trace
 name ""  xyz 330 262.4 2000
classinstance 128405 class_ref 128021 // Trace
 name "EKGtrace"  xyz 386 165.8 2000
classinstance 128789 class_ref 128021 // Trace
 name "downstream"  xyz 495.8 268 2000
classinstance 129685 class_ref 129173 // EKGView
 name ""  xyz 608 157.4 2000
linkcanvas 128533
  from ref 128021 z 2001 to point 264.6 273
  line 129045 z 2001 to ref 128277
dirscanvas 129173 z 1000 linkcanvas_ref 128533
  
  forward_label "1 logInfo()" xyz 207 244 3000
linkcanvas 128661
  from ref 128277 z 2001 to ref 128405
dirscanvas 129301 z 1000 linkcanvas_ref 128661
  
  forward_label "2 logInfo()" xyz 336 194 3000
linkcanvas 128917
  from ref 128277 z 2001 to ref 128789
dirscanvas 129429 z 1000 linkcanvas_ref 128917
  
  forward_label "3 logInfo()" xyz 406 245 3000
linkcanvas 129813
  from ref 128405 z 2001 to ref 129685
dirscanvas 129941 z 1000 linkcanvas_ref 129813
  
msgs
  msg operation_ref 129301 // "logInfo()"
    forward ranks 1 "1" dirscanvas_ref 129173
    msgs
      msg operation_ref 129301 // "logInfo()"
	forward ranks 2 "1.1" dirscanvas_ref 129301
	no_msg
      msg operation_ref 129301 // "logInfo()"
	forward ranks 3 "1.2" dirscanvas_ref 129429
	no_msg
    msgsend
msgsend
end
