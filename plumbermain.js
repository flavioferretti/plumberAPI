function myUrl(symbol, days, date, price, c_w, c_h){ 
      var getUrl = window.location; 
      var baseUrl = getUrl .protocol + "//" + getUrl.host + "/" + getUrl.pathname.split('/')[1];  
      window.location.href=baseUrl+"?Symbol="
                            +symbol+"&Observation_Days="
                            +days+"&Buy_Date="
                            +date+"&Buy_price="
                            +price+"&chart_w="
                            +c_w+"&chart_h="
                            +c_h;
}


function ispmi(){
  myUrl("ISP.MI", 300, "2020-10-09", 1.6731, 1200, 500);
}

function nflx(){
  myUrl("NFLX", 300, "2021-02-18", 548.22, 1200, 500);
}

function googl(){
  myUrl("GOOGL", 300, "2021-03-03", 2097.07, 1200, 500);
}

function amzn(){
  myUrl("AMZN", 300, "2021-02-22", 3092.93, 1200, 500);
}

function apple(){
  myUrl("AAPL", 300, "2021-02-15", 129.87, 1200, 500);
}

function tsla(){
  myUrl("TSLA", 300, "2021-02-15", 781.30, 1200, 500);
}

/*
"<a href=http://192.168.4.1:8787/p/6f5ac98c/report?Symbol=ISP.MI&Observation_Days=300&Buy_Date=2020-10-09&Buy_price=1.6731&chart_w=1200&chart_h=500>ISP.MI</a> - ",

"<a href=http://192.168.4.1:8787/p/6f5ac98c/report?Symbol=NFLX&Observation_Days=300&Buy_Date=2021-02-18&Buy_price=548.22&chart_w=1200&chart_h=500>NFLX</a> - ",

"<a href=http://192.168.4.1:8787/p/6f5ac98c/report?Symbol=GOOGL&Observation_Days=300&Buy_Date=2021-03-03&Buy_price=2097.07&chart_w=1200&chart_h=500>GOOGL</a> - ",

"<a href=http://192.168.4.1:8787/p/6f5ac98c/report?Symbol=AMZN&Observation_Days=300&Buy_Date=2021-02-22&Buy_price=3092.93&chart_w=1200&chart_h=500>AMZN</a> - ",

"<a href=http://192.168.4.1:8787/p/6f5ac98c/report?Symbol=AAPL&Observation_Days=300&Buy_Date=2021-02-15&Buy_price=129.87&chart_w=1200&chart_h=500>AAPL</a> - ",

"<a href=http://192.168.4.1:8787/p/6f5ac98c/report?Symbol=TSLA&Observation_Days=300&Buy_Date=2021-02-15&Buy_price=781.30&chart_w=1200&chart_h=500>TSLA</a> "


*/