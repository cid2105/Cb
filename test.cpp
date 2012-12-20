#include <iostream>
	
#include <string>
#include <vector>
		#include <algorithm>
			#include "mystring.h"
				#include "smartptr.h"
				using namespace std;
		// possibilities will go here
	int main(int argc, char **argv)
	{
		MyString s("jae");
		cout << f(s) << endl;
		return 0;
	}

	class Mystery {
			public:
						Mystery();
									Mystery(int m) { this->m = m; }
												int mystery(const Mystery& m1, const Mystery& m2);
														private:
																	int m;
																		};

int mystery(const Mystery& m1, const Mystery& m2) {
			return m1.m + m2.m;
						}

						MyString f(MyString &s) {
														Mystery m(3);
																Mystery t(2);
												if (mystery(m, t) == 5) {
																				return MyString("hi there");
																																		}
																			return s;
																}

