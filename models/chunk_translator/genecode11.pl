/**********************************************
* genecode.pl table 11 : bacterial
**********************************************/
genecode([t,t,t],f). genecode([t,c,t],s). genecode([t,a,t],y). genecode([t,g,t],c).
genecode([t,t,c],f). genecode([t,c,c],s). genecode([t,a,c],y). genecode([t,g,c],c).
genecode([t,t,a],l). genecode([t,c,a],s). genecode([t,a,a],*). genecode([t,g,a],*).
genecode([t,t,g],l). genecode([t,c,g],s). genecode([t,a,g],*). genecode([t,g,g],w).
                  
genecode([c,t,t],l). genecode([c,c,t],p). genecode([c,a,t],h). genecode([c,g,t],r).
genecode([c,t,c],l). genecode([c,c,c],p). genecode([c,a,c],h). genecode([c,g,c],r).
genecode([c,t,a],l). genecode([c,c,a],p). genecode([c,a,a],q). genecode([c,g,a],r).
genecode([c,t,g],l). genecode([c,c,g],p). genecode([c,a,g],q). genecode([c,g,g],r).
                  
genecode([a,t,t],i). genecode([a,c,t],t). genecode([a,a,t],n). genecode([a,g,t],s).
genecode([a,t,c],i). genecode([a,c,c],t). genecode([a,a,c],n). genecode([a,g,c],s).
genecode([a,t,a],i). genecode([a,c,a],t). genecode([a,a,a],k). genecode([a,g,a],r).
genecode([a,t,g],m). genecode([a,c,g],t). genecode([a,a,g],k). genecode([a,g,g],r).
                  
genecode([g,t,t],v). genecode([g,c,t],a). genecode([g,a,t],d). genecode([g,g,t],g).
genecode([g,t,c],v). genecode([g,c,c],a). genecode([g,a,c],d). genecode([g,g,c],g).
genecode([g,t,a],v). genecode([g,c,a],a). genecode([g,a,a],e). genecode([g,g,a],g).
genecode([g,t,g],v). genecode([g,c,g],a). genecode([g,a,g],e). genecode([g,g,g],g).