clear all
Path='C:\Users\chen-\Desktop\';
fileA="reinhardtii.xlsx";
xlsfile=strcat(Path,fileA);
data=xlsread(xlsfile,"7#","A2:CZ285");
%training and validation sets 
r=randperm(length(data));
traindata=data(r(1:218),:);
testdata=data(r(219:284),:);
trainX=traindata(:,2:103); 
trainY=traindata(:,104);
testX=testdata(:,2:103);
testY=testdata(:,104);

trainx=trainX';
trainy=trainY';
testx=testX';
testy=testY';
net=feedforwardnet(9);
net.trainFcn="trainbr";
net.divideParam.trainRatio=1;
net.divideParam.valRatio=0;
net.divideParam.testRatio=0;
net=train(net,trainx,trainy);

z=net(trainx);
t=net(testx);
Z=z';
T=t';
plotregression(trainY,Z,"Training",testY,T,"Test");
plot(trainY,Z,'O')
xlswrite("C:\Users\chen-\Desktop\Results.xlsx",trainY,"ANNresult","A2"); 
xlswrite("C:\Users\chen-\Desktop\Results.xlsx",Z,"ANNresult","B2"); 
xlswrite("C:\Users\chen-\Desktop\Results.xlsx",testY,"ANNresult","C2"); 
xlswrite("C:\Users\chen-\Desktop\Results.xlsx",T,"ANNresult","D2");
plotregression(trainY,Z,"Training",testY,T,"Test");

Path='C:\Users\chen-\Desktop\';
fileB="reinhardtii1.xlsx";
xlsfile1=strcat(Path,fileB);
data1=xlsread(xlsfile1,"6#","A1:CX1024");
data2=data1(:,1:102);
data3=data2';
r=net(data3);
R=r';
xlswrite("C:\Users\chen-\Desktop\predection.xlsx",R,"6#","A2");
