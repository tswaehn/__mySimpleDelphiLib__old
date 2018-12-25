unit colorArray;

interface
uses Graphics,Windows;

  procedure setupMyColorArray();
  var myColorArray: array[0..138] of TColor;

implementation


procedure setupMyColorArray();
begin

myColorArray[0]:=TColor(RGB(128,0,0));
myColorArray[1]:=TColor(RGB(139,0,0));
myColorArray[2]:=TColor(RGB(165,42,42));
myColorArray[3]:=TColor(RGB(178,34,34));
myColorArray[4]:=TColor(RGB(220,20,60));
myColorArray[5]:=TColor(RGB(255,0,0));
myColorArray[6]:=TColor(RGB(255,99,71));
myColorArray[7]:=TColor(RGB(255,127,80));
myColorArray[8]:=TColor(RGB(205,92,92));
myColorArray[9]:=TColor(RGB(240,128,128));
myColorArray[10]:=TColor(RGB(233,150,122));
myColorArray[11]:=TColor(RGB(250,128,114));
myColorArray[12]:=TColor(RGB(255,160,122));
myColorArray[13]:=TColor(RGB(255,69,0));
myColorArray[14]:=TColor(RGB(255,140,0));
myColorArray[15]:=TColor(RGB(255,165,0));
myColorArray[16]:=TColor(RGB(255,215,0));
myColorArray[17]:=TColor(RGB(184,134,11));
myColorArray[18]:=TColor(RGB(218,165,32));
myColorArray[19]:=TColor(RGB(238,232,170));
myColorArray[20]:=TColor(RGB(189,183,107));
myColorArray[21]:=TColor(RGB(240,230,140));
myColorArray[22]:=TColor(RGB(128,128,0));
myColorArray[23]:=TColor(RGB(255,255,0));
myColorArray[24]:=TColor(RGB(154,205,50));
myColorArray[25]:=TColor(RGB(85,107,47));
myColorArray[26]:=TColor(RGB(107,142,35));
myColorArray[27]:=TColor(RGB(124,252,0));
myColorArray[28]:=TColor(RGB(127,255,0));
myColorArray[29]:=TColor(RGB(173,255,47));
myColorArray[30]:=TColor(RGB(0,100,0));
myColorArray[31]:=TColor(RGB(0,128,0));
myColorArray[32]:=TColor(RGB(34,139,34));
myColorArray[33]:=TColor(RGB(0,255,0));
myColorArray[34]:=TColor(RGB(50,205,50));
myColorArray[35]:=TColor(RGB(144,238,144));
myColorArray[36]:=TColor(RGB(152,251,152));
myColorArray[37]:=TColor(RGB(143,188,143));
myColorArray[38]:=TColor(RGB(0,250,154));
myColorArray[39]:=TColor(RGB(0,255,127));
myColorArray[40]:=TColor(RGB(46,139,87));
myColorArray[41]:=TColor(RGB(102,205,170));
myColorArray[42]:=TColor(RGB(60,179,113));
myColorArray[43]:=TColor(RGB(32,178,170));
myColorArray[44]:=TColor(RGB(47,79,79));
myColorArray[45]:=TColor(RGB(0,128,128));
myColorArray[46]:=TColor(RGB(0,139,139));
myColorArray[47]:=TColor(RGB(0,255,255));
myColorArray[48]:=TColor(RGB(0,255,255));
myColorArray[49]:=TColor(RGB(224,255,255));
myColorArray[50]:=TColor(RGB(0,206,209));
myColorArray[51]:=TColor(RGB(64,224,208));
myColorArray[52]:=TColor(RGB(72,209,204));
myColorArray[53]:=TColor(RGB(175,238,238));
myColorArray[54]:=TColor(RGB(127,255,212));
myColorArray[55]:=TColor(RGB(176,224,230));
myColorArray[56]:=TColor(RGB(95,158,160));
myColorArray[57]:=TColor(RGB(70,130,180));
myColorArray[58]:=TColor(RGB(100,149,237));
myColorArray[59]:=TColor(RGB(0,191,255));
myColorArray[60]:=TColor(RGB(30,144,255));
myColorArray[61]:=TColor(RGB(173,216,230));
myColorArray[62]:=TColor(RGB(135,206,235));
myColorArray[63]:=TColor(RGB(135,206,250));
myColorArray[64]:=TColor(RGB(25,25,112));
myColorArray[65]:=TColor(RGB(0,0,128));
myColorArray[66]:=TColor(RGB(0,0,139));
myColorArray[67]:=TColor(RGB(0,0,205));
myColorArray[68]:=TColor(RGB(0,0,255));
myColorArray[69]:=TColor(RGB(65,105,225));
myColorArray[70]:=TColor(RGB(138,43,226));
myColorArray[71]:=TColor(RGB(75,0,130));
myColorArray[72]:=TColor(RGB(72,61,139));
myColorArray[73]:=TColor(RGB(106,90,205));
myColorArray[74]:=TColor(RGB(123,104,238));
myColorArray[75]:=TColor(RGB(147,112,219));
myColorArray[76]:=TColor(RGB(139,0,139));
myColorArray[77]:=TColor(RGB(148,0,211));
myColorArray[78]:=TColor(RGB(153,50,204));
myColorArray[79]:=TColor(RGB(186,85,211));
myColorArray[80]:=TColor(RGB(128,0,128));
myColorArray[81]:=TColor(RGB(216,191,216));
myColorArray[82]:=TColor(RGB(221,160,221));
myColorArray[83]:=TColor(RGB(238,130,238));
myColorArray[84]:=TColor(RGB(255,0,255));
myColorArray[85]:=TColor(RGB(218,112,214));
myColorArray[86]:=TColor(RGB(199,21,133));
myColorArray[87]:=TColor(RGB(219,112,147));
myColorArray[88]:=TColor(RGB(255,20,147));
myColorArray[89]:=TColor(RGB(255,105,180));
myColorArray[90]:=TColor(RGB(255,182,193));
myColorArray[91]:=TColor(RGB(255,192,203));
myColorArray[92]:=TColor(RGB(250,235,215));
myColorArray[93]:=TColor(RGB(245,245,220));
myColorArray[94]:=TColor(RGB(255,228,196));
myColorArray[95]:=TColor(RGB(255,235,205));
myColorArray[96]:=TColor(RGB(245,222,179));
myColorArray[97]:=TColor(RGB(255,248,220));
myColorArray[98]:=TColor(RGB(255,250,205));
myColorArray[99]:=TColor(RGB(250,250,210));
myColorArray[100]:=TColor(RGB(255,255,224));
myColorArray[101]:=TColor(RGB(139,69,19));
myColorArray[102]:=TColor(RGB(160,82,45));
myColorArray[103]:=TColor(RGB(210,105,30));
myColorArray[104]:=TColor(RGB(205,133,63));
myColorArray[105]:=TColor(RGB(244,164,96));
myColorArray[106]:=TColor(RGB(222,184,135));
myColorArray[107]:=TColor(RGB(210,180,140));
myColorArray[108]:=TColor(RGB(188,143,143));
myColorArray[109]:=TColor(RGB(255,228,181));
myColorArray[110]:=TColor(RGB(255,222,173));
myColorArray[111]:=TColor(RGB(255,218,185));
myColorArray[112]:=TColor(RGB(255,228,225));
myColorArray[113]:=TColor(RGB(255,240,245));
myColorArray[114]:=TColor(RGB(250,240,230));
myColorArray[115]:=TColor(RGB(253,245,230));
myColorArray[116]:=TColor(RGB(255,239,213));
myColorArray[117]:=TColor(RGB(255,245,238));
myColorArray[118]:=TColor(RGB(245,255,250));
myColorArray[119]:=TColor(RGB(112,128,144));
myColorArray[120]:=TColor(RGB(119,136,153));
myColorArray[121]:=TColor(RGB(176,196,222));
myColorArray[122]:=TColor(RGB(230,230,250));
myColorArray[123]:=TColor(RGB(255,250,240));
myColorArray[124]:=TColor(RGB(240,248,255));
myColorArray[125]:=TColor(RGB(248,248,255));
myColorArray[126]:=TColor(RGB(240,255,240));
myColorArray[127]:=TColor(RGB(255,255,240));
myColorArray[128]:=TColor(RGB(240,255,255));
myColorArray[129]:=TColor(RGB(255,250,250));
myColorArray[130]:=TColor(RGB(0,0,0));
myColorArray[131]:=TColor(RGB(105,105,105));
myColorArray[132]:=TColor(RGB(128,128,128));
myColorArray[133]:=TColor(RGB(169,169,169));
myColorArray[134]:=TColor(RGB(192,192,192));
myColorArray[135]:=TColor(RGB(211,211,211));
myColorArray[136]:=TColor(RGB(220,220,220));
myColorArray[137]:=TColor(RGB(245,245,245));
myColorArray[138]:=TColor(RGB(255,255,255));



end;



end.
