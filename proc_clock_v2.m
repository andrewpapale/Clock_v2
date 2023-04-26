% 2023-04-19 AndyP
% Test Clock Pilot data


nT = height(df);
trialcode = [];
rewFunc = [];
mush = [];

mag = df.mag';
freq = df.freq';
ev = df.ev';
rt = df.rt_shifted';
inc = df.inc';

for iT=1:nT
    if df.trialcode(iT)=='experiment_U' | df.trialcode(iT)=='experiment_noU'
        trialcode(iT) = 1;
    elseif df.trialcode(iT)=='dispFeedback_U' | df.trialcode(iT)=='dispFeedback_noU'
        trialcode(iT)=2;
    end
    
    if df.Earnings(iT)==0.5
        mush(iT) = 1;
    elseif df.Earnings(iT)==1
        mush(iT) = 2;
    elseif df.Earnings(iT)==1.5
        mush(iT)=3;
    elseif df.Earnings(iT)==2
        mush(iT)=4;
    elseif df.Earnings(iT)==2.5
        mush(iT)=5;
    elseif df.Earnings(iT)==3
        mush(iT)=6;
    elseif df.Earnings(iT)==3.5
        mush(iT)=7;
    elseif df.Earnings(iT)==4
        mush(iT)=8;
    elseif df.Earnings(iT)==4.5
        mush(iT)=9;
    elseif df.Earnings(iT)==5
        mush(iT)=10;
    end
    
    if df.scrfunc(iT)=='IEV'
        rewFunc(iT) = 1;
    elseif df.scrfunc(iT)=='DEV'
        rewFunc(iT) = 2;
    elseif df.scrfunc(iT)=='CEV'
        rewFunc(iT) = 3;
    elseif df.scrfunc(iT)=='CEVR'
        rewFunc(iT) = 4;
    end
end
        

