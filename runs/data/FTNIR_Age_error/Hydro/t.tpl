DATA_SECTION
  init_int na
  init_matrix adf(1,na,1,3)
	init_int nl
  init_matrix ldf(1,nl,1,3)
	matrix alk(1,100,1,15)
	matrix galk(1,100,1,15)
	vector lf(1,100)
	vector yrs(1,4)
 LOCAL_CALCS
   yrs(1) = 2014;
   yrs(2) = 2016;
   yrs(3) = 2018;
   yrs(4) = 2022;
   galk.initialize();
	 for (int i=1;i<=na;i++)
		galk(adf(i,3),adf(i,2))++;
	 for (int k=1;k<=4;k++)
	 {
	   lf.initialize();
     alk.initialize();
	   for (int i=1;i<=na;i++)
				if(adf(i,1)==yrs(k))
					alk(adf(i,3),adf(i,2))++;
	   for (int i=1;i<=nl;i++)
				if(ldf(i,1)==yrs(k))
					lf(ldf(i,2)) = ldf(i,3);
		for (int i=1;i<=100;i++)
			if(sum(alk(i))>0)
				alk(i) /= sum(alk(i));
			else
			  if(sum(galk(i))>0)
				  alk(i) = galk(i)/sum(galk(i));
			cout<<"--------------------------------"<<endl;
			cout<<yrs(k) <<endl;
			//cout<<alk<<endl;
			cout<<sum(lf)<<endl;
			cout<<sum(lf*alk)<<endl;
			cout<<lf*alk<<endl;
			cout<<"--------------------------------"<<endl;
	 }

       
	 // for (int j=1;j<=nl;j++)
		exit(1);
 END_CALCS

PARAMETER_SECTION
  init_number b0
  init_number b1
	init_number bb1
  objective_function_value f
PROCEDURE_SECTION

