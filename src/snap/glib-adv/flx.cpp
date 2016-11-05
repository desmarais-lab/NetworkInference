/////////////////////////////////////////////////
// Check-Sum
const int TCs::MxMask=0x0FFFFFFF;

TCs TCs::GetCsFromBf(char* Bf, const int& BfL){
  TCs Cs;
  for (int BfC=0; BfC<BfL; BfC++){Cs+=Bf[BfC];}
  return Cs;
}

/////////////////////////////////////////////////
// Stream-Base
TStr TSBase::GetSNm() const {
  return TStr(SNm.CStr());
}

/////////////////////////////////////////////////
// Input-Stream
TSIn::TSIn(const TStr& Str):
  TSBase(Str.CStr()), FastMode(false){}

void TSIn::LoadCs(){
  TCs CurCs=Cs; TCs TestCs;
  Cs+=GetBf(&TestCs, sizeof(TestCs));
  EAssertR(CurCs==TestCs, "Invalid checksum reading '"+GetSNm()+"'.");
}

void TSIn::Load(char*& CStr){
  char Ch; Load(Ch);
  int CStrLen=int(Ch);
  EAssertR(CStrLen>=0, "Error reading stream '"+GetSNm()+"'.");
  CStr=new char[CStrLen+1];
  if (CStrLen>0){Cs+=GetBf(CStr, CStrLen);}
  CStr[CStrLen]=TCh::NullCh;
}

const PSIn TSIn::StdIn=PSIn(new TStdIn());

/////////////////////////////////////////////////
// Output-Stream
TSOut::TSOut(const TStr& Str):
  TSBase(Str.CStr()), MxLnLen(-1), LnLen(0){}

int TSOut::UpdateLnLen(const int& StrLen, const bool& ForceInLn){
  int Cs=0;
  if (MxLnLen!=-1){
    if ((!ForceInLn)&&(LnLen+StrLen>MxLnLen)){Cs+=PutLn();}
    LnLen+=StrLen;
  }
  return Cs;
}

int TSOut::PutMem(const TMem& Mem){
  return PutBf(Mem(), Mem.Len());
}

int TSOut::PutCh(const char& Ch, const int& Chs){
  int Cs=0;
  for (int ChN=0; ChN<Chs; ChN++){Cs+=PutCh(Ch);}
  return Cs;
}

int TSOut::PutBool(const bool& Bool){
  return PutStr(TBool::GetStr(Bool));
}

int TSOut::PutInt(const int& Int){
  return PutStr(TInt::GetStr(Int));
}

int TSOut::PutInt(const int& Int, char* FmtStr){
  return PutStr(TInt::GetStr(Int, FmtStr));
}

int TSOut::PutUInt(const uint& UInt){
  return PutStr(TUInt::GetStr(UInt));
}

int TSOut::PutUInt(const uint& UInt, char* FmtStr){
  return PutStr(TUInt::GetStr(UInt, FmtStr));
}

int TSOut::PutFlt(const double& Flt){
  return PutStr(TFlt::GetStr(Flt));
}

int TSOut::PutFlt(const double& Flt, char* FmtStr){
  return PutStr(TFlt::GetStr(Flt, FmtStr));
}

int TSOut::PutStr(const char* CStr){
  int Cs=UpdateLnLen(int(strlen(CStr)));
  return Cs+PutBf(CStr, int(strlen(CStr)));
}

int TSOut::PutStr(const TChA& ChA){
  int Cs=UpdateLnLen(ChA.Len());
  return Cs+PutBf(ChA.CStr(), ChA.Len());
}

int TSOut::PutStr(const TStr& Str, char* FmtStr){
  return PutStr(TStr::GetStr(Str, FmtStr));
}

int TSOut::PutStr(const TStr& Str, const bool& ForceInLn){
  int Cs=UpdateLnLen(Str.Len(), ForceInLn);
  return Cs+PutBf(Str.CStr(), Str.Len());
}

int TSOut::PutIndent(const int& IndentLev){
  return PutCh(' ', IndentLev*2);
}

int TSOut::PutLn(const int& Lns){
  LnLen=0; int Cs=0;
  for (int LnN=0; LnN<Lns; LnN++){Cs+=PutCh('\n');}
  return Cs;
}

int TSOut::PutDosLn(const int& Lns){
  LnLen=0; int Cs=0;
  for (int LnN=0; LnN<Lns; LnN++){Cs+=PutCh(TCh::CrCh)+PutCh(TCh::LfCh);}
  return Cs;
}

int TSOut::PutSep(const int& NextStrLen){
  int Cs=0;
  if (MxLnLen==-1){
    Cs+=PutCh(' ');
  } else {
    if (LnLen>0){
      if (LnLen+1+NextStrLen>MxLnLen){Cs+=PutLn();} else {Cs+=PutCh(' ');}
    }
  }
  return Cs;
}

int TSOut::PutSepLn(const int& Lns){
  int Cs=0;
  if (LnLen>0){Cs+=PutLn();}
  Cs+=PutLn(Lns);
  return Cs;
}

void TSOut::Save(const char* CStr){
  int CStrLen=int(strlen(CStr));
  EAssertR(CStrLen<=127, "Error writting stream '"+GetSNm()+"'.");
  Save(char(CStrLen));
  if (CStrLen>0){Cs+=PutBf(CStr, CStrLen);}
}

void TSOut::Save(TSIn& SIn, const int& BfL){
  if (BfL==-1){
    while (!SIn.Eof()){Save(SIn.GetCh());}
  } else {
    for (int BfC=0; BfC<BfL; BfC++){Save(SIn.GetCh());}
  }
}

TSOut& TSOut::operator<<(TSIn& SIn) {
  while (!SIn.Eof())
    operator<<((char)SIn.GetCh());
  return *this;
}

const PSOut TSOut::StdOut=PSOut(new TStdOut());

/////////////////////////////////////////////////
// Standard-Input
int TStdIn::GetBf(const void* LBf, const int& LBfL){
  int LBfS=0;
  for (int LBfC=0; LBfC<LBfL; LBfC++){
    LBfS+=(((char*)LBf)[LBfC]=GetCh());}
  return LBfS;
}

/////////////////////////////////////////////////
// Standard-Output
int TStdOut::PutBf(const void* LBf, const int& LBfL){
  int LBfS=0;
  for (int LBfC=0; LBfC<LBfL; LBfC++){
    LBfS+=PutCh(((char*)LBf)[LBfC]);}
  return LBfS;
}

/////////////////////////////////////////////////
// Input-File
const int TFIn::MxBfL=16*1024;

void TFIn::SetFPos(const int& FPos) const {
  EAssertR(
   fseek(FileId, FPos, SEEK_SET)==0,
   "Error seeking into file '"+GetSNm()+"'.");
}

int TFIn::GetFPos() const {
  const int FPos=ftell(FileId);
  EAssertR(FPos!=-1, "Error seeking into file '"+GetSNm()+"'.");
  return FPos;
}

int TFIn::GetFLen() const {
  const int FPos=GetFPos();
  EAssertR(
   fseek(FileId, 0, SEEK_END)==0,
   "Error seeking into file '"+GetSNm()+"'.");
  const int FLen=GetFPos(); SetFPos(FPos);
  return FLen;
}

void TFIn::FillBf(){
  EAssertR(
   (BfC==BfL)&&((BfL==-1)||(BfL==MxBfL)),
   "Error reading file '"+GetSNm()+"'.");
  BfL=int(fread(Bf, 1, MxBfL, FileId));
  EAssertR((BfC!=0)||(BfL!=0), "Error reading file '"+GetSNm()+"'.");
  BfC=0;
}

TFIn::TFIn(const TStr& FNm):
  TSIn(FNm), FileId(NULL), Bf(NULL), BfC(0), BfL(0){
  EAssertR(!FNm.Empty(), "Empty file-name.");
  FileId=fopen(FNm.CStr(), "rb");
  EAssertR(FileId!=NULL, "Can not open file '"+FNm+"'.");
  Bf=new char[MxBfL]; BfC=BfL=-1; FillBf();
}

TFIn::TFIn(const TStr& FNm, bool& OpenedP):
  TSIn(FNm), FileId(NULL), Bf(NULL), BfC(0), BfL(0){
  EAssertR(!FNm.Empty(), "Empty file-name.");
  FileId=fopen(FNm.CStr(), "rb");
  OpenedP=(FileId!=NULL);
  if (OpenedP){
    Bf=new char[MxBfL]; BfC=BfL=-1; FillBf();}
}

PSIn TFIn::New(const TStr& FNm){
  return PSIn(new TFIn(FNm));
}

PSIn TFIn::New(const TStr& FNm, bool& OpenedP){
  return PSIn(new TFIn(FNm, OpenedP));
}

TFIn::~TFIn(){
  if (FileId!=NULL){
    EAssertR(fclose(FileId)==0, "Can not close file '"+GetSNm()+"'.");}
  if (Bf!=NULL){delete[] Bf;}
}

int TFIn::GetBf(const void* LBf, const int& LBfL){
  int LBfS=0;
  if (BfC+LBfL>BfL){
    for (int LBfC=0; LBfC<LBfL; LBfC++){
      if (BfC==BfL){FillBf();}
      LBfS+=((char*)LBf)[LBfC]=Bf[BfC++];}
  } else {
    for (int LBfC=0; LBfC<LBfL; LBfC++){
      LBfS+=(((char*)LBf)[LBfC]=Bf[BfC++]);}
  }
  return LBfS;
}

/////////////////////////////////////////////////
// Output-File
const int TFOut::MxBfL=16*1024;;

void TFOut::FlushBf(){
  EAssertR(
   (int)fwrite(Bf, 1, BfL, FileId)==BfL,
   "Error writting to the file '"+GetSNm()+"'.");
  BfL=0;
}

TFOut::TFOut(const TStr& FNm, const bool& Append):
  TSOut(FNm), FileId(NULL), Bf(NULL), BfL(0){
  if (FNm.GetUc()=="CON"){
    FileId=stdout;
  } else {
    if (Append){FileId=fopen(FNm.CStr(), "a+b");}
    else {FileId=fopen(FNm.CStr(), "w+b");}
    EAssertR(FileId!=NULL, "Can not open file '"+FNm+"'.");
    Bf=new char[MxBfL]; BfL=0;
  }
}

TFOut::TFOut(const TStr& FNm, const bool& Append, bool& OpenedP):
  TSOut(FNm), FileId(NULL), Bf(NULL), BfL(0){
  if (FNm.GetUc()=="CON"){
    FileId=stdout;
  } else {
    if (Append){FileId=fopen(FNm.CStr(), "a+b");}
    else {FileId=fopen(FNm.CStr(), "w+b");}
    OpenedP=(FileId!=NULL);
    if (OpenedP){
      Bf=new char[MxBfL]; BfL=0;}
  }
}

PSOut TFOut::New(const TStr& FNm, const bool& Append){
  return PSOut(new TFOut(FNm, Append));
}

PSOut TFOut::New(const TStr& FNm, const bool& Append, bool& OpenedP){
  PSOut SOut=PSOut(new TFOut(FNm, Append, OpenedP));
  if (OpenedP){return SOut;} else {return NULL;}
}

TFOut::~TFOut(){
  if (FileId!=NULL){FlushBf();}
  if (Bf!=NULL){delete[] Bf;}
  if (FileId!=NULL){
    EAssertR(fclose(FileId)==0, "Can not close file '"+GetSNm()+"'.");}
}

int TFOut::PutCh(const char& Ch){
  if (BfL==MxBfL){FlushBf();}
  return Bf[BfL++]=Ch;
}

int TFOut::PutBf(const void* LBf, const int& LBfL){
  int LBfS=0;
  if (BfL+LBfL>MxBfL){
    for (int LBfC=0; LBfC<LBfL; LBfC++){
      LBfS+=PutCh(((char*)LBf)[LBfC]);}
  } else {
    for (int LBfC=0; LBfC<LBfL; LBfC++){
      LBfS+=(Bf[BfL++]=((char*)LBf)[LBfC]);}
  }
  return LBfS;
}

void TFOut::Flush(){
  FlushBf();
  EAssertR(fflush(FileId)==0, "Can not flush file '"+GetSNm()+"'.");
}

/////////////////////////////////////////////////
// Input-Output-File
TFInOut::TFInOut(const TStr& FNm, const TFAccess& FAccess, const bool& CreateIfNo) :
 TSBase(TSStr(FNm.CStr())), FileId(NULL) {
  switch (FAccess){
    case faCreate: FileId=fopen(FNm.CStr(), "w+b"); break;
    case faUpdate: FileId=fopen(FNm.CStr(), "r+b"); break;
    case faAppend: FileId=fopen(FNm.CStr(), "r+b");
      if (FileId!=NULL){fseek(FileId, SEEK_END, 0);} break;
    case faRdOnly: FileId=fopen(FNm.CStr(), "rb"); break;
    default: Fail;
  }
  if ((FileId==NULL)&&(CreateIfNo)){FileId=fopen(FNm.CStr(), "w+b");}
  IAssert(FileId!=NULL);
}

PSInOut TFInOut::New(const TStr& FNm, const TFAccess& FAccess, const bool& CreateIfNo) {
  return PSInOut(new TFInOut(FNm, FAccess, CreateIfNo));
}

int TFInOut::GetSize() const {
  const int FPos = GetPos();
  IAssert(fseek(FileId, 0, SEEK_END) == 0);
  const int FLen = GetPos();
  IAssert(fseek(FileId, FPos, SEEK_SET) == 0);
  return FLen;
}

int TFInOut::PutBf(const void* LBf, const int& LBfL) {
  int LBfS = 0;
  for (int i = 0; i < LBfL; i++) {
    LBfS += ((char *)LBf)[i];
  }
  IAssert(fwrite(LBf, sizeof(char), LBfL, FileId) == (size_t) LBfL);
  return LBfS;;
}

int TFInOut::GetBf(const void* LBf, const int& LBfL) {
  IAssert(fread((void *)LBf, sizeof(char), LBfL, FileId) == (size_t) LBfL);
  int LBfS = 0;
  for (int i = 0; i < LBfL; i++) {
    LBfS += ((char *)LBf)[i];
  }
  return LBfS;
}

TStr TFInOut::GetFNm() const {
  return GetSNm();
}

/////////////////////////////////////////////////
// Input-Memory
TMIn::TMIn(const void* _Bf, const int& _BfL, const bool& TakeBf, const bool& _Free):
  TSIn("Input-Memory"), Bf(NULL), BfC(0), BfL(_BfL), Free(_Free){
  if (TakeBf){
    Bf=(char*)_Bf;
  } else {
    Bf=new char[BfL]; memmove(Bf, _Bf, BfL);
  }
}

TMIn::TMIn(TSIn& SIn):
  TSIn("Input-Memory"), Bf(NULL), BfC(0), BfL(0){
  BfL=SIn.Len(); Bf=new char[BfL];
  for (int BfC=0; BfC<BfL; BfC++){Bf[BfC]=SIn.GetCh();}
}

TMIn::TMIn(const char* CStr):
  TSIn("Input-Memory"), Bf(NULL), BfC(0), BfL(0){
  BfL=int(strlen(CStr)); Bf=new char[BfL+1]; strcpy(Bf, CStr);
}

TMIn::TMIn(const TStr& Str):
  TSIn("Input-Memory"), Bf(NULL), BfC(0), BfL(0){
  BfL=Str.Len(); Bf=new char[BfL]; strncpy(Bf, Str.CStr(), BfL);
}

TMIn::TMIn(const TChA& ChA):
  TSIn("Input-Memory"), Bf(NULL), BfC(0), BfL(0){
  BfL=ChA.Len(); Bf=new char[BfL]; strncpy(Bf, ChA.CStr(), BfL);
}

PSIn TMIn::New(const char* CStr){
  return PSIn(new TMIn(CStr));
}

PSIn TMIn::New(const TStr& Str){
  return PSIn(new TMIn(Str));
}

PSIn TMIn::New(const TChA& ChA){
  return PSIn(new TMIn(ChA));
}

char TMIn::GetCh(){
  EAssertR(BfC<BfL, "Reading beyond the end of stream.");
  return Bf[BfC++];
}

char TMIn::PeekCh(){
  EAssertR(BfC<BfL, "Reading beyond the end of stream.");
  return Bf[BfC];
}

int TMIn::GetBf(const void* LBf, const int& LBfL){
  EAssertR(BfC+LBfL<=BfL, "Reading beyond the end of stream.");
  int LBfS=0;
  for (int LBfC=0; LBfC<LBfL; LBfC++){
    LBfS+=(((char*)LBf)[LBfC]=Bf[BfC++]);}
  return LBfS;
}

/////////////////////////////////////////////////
// Output-Memory
void TMOut::Resize(){
  IAssert(OwnBf&&(BfL==MxBfL));
  if (Bf==NULL){
    IAssert(MxBfL==0); Bf=new char[MxBfL=1024];
  } else {
    MxBfL*=2; char* NewBf=new char[MxBfL];
    memmove(NewBf, Bf, BfL); delete[] Bf; Bf=NewBf;
  }
}

TMOut::TMOut(const int& _MxBfL):
  TSOut("Output-Memory"),
  Bf(NULL), BfL(0), MxBfL(0), OwnBf(true){
  MxBfL=_MxBfL>0?_MxBfL:1024;
  Bf=new char[MxBfL];
}

TMOut::TMOut(char* _Bf, const int& _MxBfL):
  TSOut("Output-Memory"),
  Bf(_Bf), BfL(0), MxBfL(_MxBfL), OwnBf(false){}

int TMOut::PutBf(const void* LBf, const int& LBfL){
  int LBfS=0;
  if (BfL+LBfL>MxBfL){
    for (int LBfC=0; LBfC<LBfL; LBfC++){
      LBfS+=PutCh(((char*)LBf)[LBfC]);}
  } else {
    for (int LBfC=0; LBfC<LBfL; LBfC++){
      LBfS+=(Bf[BfL++]=((char*)LBf)[LBfC]);}
  }
  return LBfS;
}

TStr TMOut::GetAsStr() const {
  TChA ChA(BfL);
  for (int BfC=0; BfC<BfL; BfC++){ChA+=Bf[BfC];}
  return ChA;
}

void TMOut::CutBf(const int& CutBfL){
  IAssert((0<=CutBfL)&&(CutBfL<=BfL));
  if (CutBfL==BfL){BfL=0;}
  else {memmove(Bf, Bf+CutBfL, BfL-CutBfL); BfL=BfL-CutBfL;}
}

PSIn TMOut::GetSIn(const bool& IsCut, const int& CutBfL){
  IAssert((CutBfL==-1)||((0<=CutBfL)));
  int SInBfL= (CutBfL==-1) ? BfL : TInt::GetMn(BfL, CutBfL);
  PSIn SIn;
  if (OwnBf&&IsCut&&(SInBfL==BfL)){
    SIn=PSIn(new TMIn(Bf, SInBfL, true));
    Bf=NULL; BfL=MxBfL=0; OwnBf=true;
  } else {
    SIn=PSIn(new TMIn(Bf, SInBfL, false));
    if (IsCut){CutBf(SInBfL);}
  }
  return SIn;
}

bool TMOut::IsCrLfLn() const {
  for (int BfC=0; BfC<BfL; BfC++){
    if ((Bf[BfC]==TCh::CrCh)&&((BfC+1<BfL)&&(Bf[BfC+1]==TCh::LfCh))){return true;}}
  return false;
}

TStr TMOut::GetCrLfLn(){
  IAssert(IsCrLfLn());
  TChA Ln;
  for (int BfC=0; BfC<BfL; BfC++){
    char Ch=Bf[BfC];
    if ((Ch==TCh::CrCh)&&((BfC+1<BfL)&&(Bf[BfC+1]==TCh::LfCh))){
      Ln+=TCh::CrCh; Ln+=TCh::LfCh; CutBf(BfC+1+1); break;
    } else {
      Ln+=Ch;
    }
  }
  return Ln;
}

bool TMOut::IsEolnLn() const {
  for (int BfC=0; BfC<BfL; BfC++){
    if ((Bf[BfC]==TCh::CrCh)||(Bf[BfC]==TCh::LfCh)){return true;}
  }
  return false;
}

TStr TMOut::GetEolnLn(const bool& DoAddEoln, const bool& DoCutBf){
  IAssert(IsEolnLn());
  int LnChs=0; TChA Ln;
  for (int BfC=0; BfC<BfL; BfC++){
    char Ch=Bf[BfC];
    if ((Ch==TCh::CrCh)||(Ch==TCh::LfCh)){
      LnChs++; if (DoAddEoln){Ln+=Ch;}
      if (BfC+1<BfL){
        char NextCh=Bf[BfC+1];
        if (((Ch==TCh::CrCh)&&(NextCh==TCh::LfCh))||
         ((Ch==TCh::LfCh)&&(NextCh==TCh::CrCh))){
          LnChs++; if (DoAddEoln){Ln+=NextCh;}
        }
      }
      break;
    } else {
      LnChs++; Ln+=Ch;
    }
  }
  if (DoCutBf){
    CutBf(LnChs);
  }
  return Ln;
}

void TMOut::MkEolnLn(){
  if (!IsEolnLn()){
    PutCh(TCh::CrCh); PutCh(TCh::LfCh);}
}

/////////////////////////////////////////////////
// fseek-Constants-Definitions
// because of strange Borland CBuilder behaviour in sysdefs.h
#ifndef SEEK_SET
#define SEEK_CUR    1
#define SEEK_END    2
#define SEEK_SET    0
#endif

/////////////////////////////////////////////////
// Random-File
void TFRnd::RefreshFPos(){
  EAssertR(
   fseek(FileId, 0, SEEK_CUR)==0,
   "Error seeking into file '"+FNm+"'.");
}

TFRnd::TFRnd(const TStr& _FNm, const TFAccess& FAccess,
 const bool& CreateIfNo, const int& _HdLen, const int& _RecLen):
  FileId(NULL), FNm(_FNm.CStr()),
  RecAct(false), HdLen(_HdLen), RecLen(_RecLen){
  RecAct=(HdLen>=0)&&(RecLen>0);
  switch (FAccess){
    case faCreate: FileId=fopen(FNm.CStr(), "w+b"); break;
    case faUpdate: FileId=fopen(FNm.CStr(), "r+b"); break;
    case faAppend: FileId=fopen(FNm.CStr(), "r+b");
      if (FileId!=NULL){fseek(FileId, SEEK_END, 0);} break;
    case faRdOnly: FileId=fopen(FNm.CStr(), "rb"); break;
    default: Fail;
  }
  if ((FileId==NULL)&&(CreateIfNo)){
    FileId=fopen(FNm.CStr(), "w+b");}
  EAssertR(FileId!=NULL, "Can not open file '"+_FNm+"'.");
}

TFRnd::~TFRnd(){
  EAssertR(fclose(FileId)==0, "Can not close file '"+FNm+"'.");
}

TStr TFRnd::GetFNm() const {
  return FNm.CStr();
}

void TFRnd::SetFPos(const int& FPos){
  EAssertR(
   fseek(FileId, FPos, SEEK_SET)==0,
   "Error seeking into file '"+FNm+"'.");
}

void TFRnd::MoveFPos(const int& DFPos){
  EAssertR(
   fseek(FileId, DFPos, SEEK_CUR)==0,
   "Error seeking into file '"+FNm+"'.");
}

int TFRnd::GetFPos(){
  int FPos=ftell(FileId);
  EAssertR(FPos!=-1, "Error seeking into file '"+FNm+"'.");
  return FPos;
}

int TFRnd::GetFLen(){
  int FPos=GetFPos();
  EAssertR(
   fseek(FileId, 0, SEEK_END)==0,
   "Error seeking into file '"+FNm+"'.");
  int FLen=GetFPos(); SetFPos(FPos); return FLen;
}

void TFRnd::SetRecN(const int& RecN){
  IAssert(RecAct);
  SetFPos(HdLen+RecN*RecLen);
}

int TFRnd::GetRecN(){
  IAssert(RecAct);
  int FPos=GetFPos()-HdLen;
  EAssertR(FPos%RecLen==0, "Invalid position in file'"+FNm+"'.");
  return FPos/RecLen;
}

int TFRnd::GetRecs(){
  IAssert(RecAct);
  int FLen=GetFLen()-HdLen;
  EAssertR(FLen%RecLen==0, "Invalid length of file'"+FNm+"'.");
  return FLen/RecLen;
}

void TFRnd::GetBf(void* Bf, const int& BfL){
  RefreshFPos();
  EAssertR(
   int(fread(Bf, 1, BfL, FileId))==BfL,
   "Error reading file '"+FNm+"'.");
}

void TFRnd::PutBf(const void* Bf, const int& BfL){
  RefreshFPos();
  EAssertR(
   int(fwrite(Bf, 1, BfL, FileId))==BfL,
   "Error writting to the file '"+FNm+"'.");
}

void TFRnd::Flush(){
  EAssertR(fflush(FileId)==0, "Can not flush file '"+FNm+"'.");
}

void TFRnd::PutCh(const char& Ch, const int& Chs){
  if (Chs>0){
    char* CStr=new char[Chs];
    for (int ChN=0; ChN<Chs; ChN++){CStr[ChN]=Ch;}
    PutBf(CStr, Chs);
    delete[] CStr;
  }
}

void TFRnd::PutStr(const TStr& Str){
  PutBf(Str.CStr(), Str.Len()+1);
}

TStr TFRnd::GetStr(const int& StrLen, bool& IsOk){
  IsOk=false; TStr Str;
  if (GetFPos()+StrLen+1<=GetFLen()){
    char* CStr=new char[StrLen+1];
    GetBf(CStr, StrLen+1);
    if (CStr[StrLen+1-1]==TCh::NullCh){IsOk=true; Str=CStr;}
    delete[] CStr;
  }
  return Str;
}

TStr TFRnd::GetStr(const int& StrLen){
  TStr Str;
  char* CStr=new char[StrLen+1];
  GetBf(CStr, StrLen+1);
  EAssertR(CStr[StrLen+1-1]==TCh::NullCh, "Error reading file '"+FNm+"'.");
  Str=CStr;
  delete[] CStr;
  return Str;
}

void TFRnd::PutSIn(const PSIn& SIn, TCs& Cs){
  int BfL=SIn->Len();
  char* Bf=new char[BfL];
  SIn->GetBf(Bf, BfL);
  Cs=TCs::GetCsFromBf(Bf, BfL);
  PutBf(Bf, BfL);
  delete[] Bf;
}

PSIn TFRnd::GetSIn(const int& BfL, TCs& Cs){
  char* Bf=new char[BfL];
  GetBf(Bf, BfL);
  Cs=TCs::GetCsFromBf(Bf, BfL);
  PSIn SIn=PSIn(new TMIn(Bf, BfL, true));
  return SIn;
}

TStr TFRnd::GetStrFromFAccess(const TFAccess& FAccess){
  switch (FAccess){
    case faCreate: return "Create";
    case faUpdate: return "Update";
    case faAppend: return "Append";
    case faRdOnly: return "ReadOnly";
    case faRestore: return "Restore";
    default: Fail; return TStr();
  }
}

TFAccess TFRnd::GetFAccessFromStr(const TStr& Str){
  TStr UcStr=Str.GetUc();
  if (UcStr=="CREATE"){return faCreate;}
  if (UcStr=="UPDATE"){return faUpdate;}
  if (UcStr=="APPEND"){return faAppend;}
  if (UcStr=="READONLY"){return faRdOnly;}
  if (UcStr=="RESTORE"){return faRestore;}

  if (UcStr=="NEW"){return faCreate;}
  if (UcStr=="CONT"){return faUpdate;}
  if (UcStr=="CONTINUE"){return faUpdate;}
  if (UcStr=="REST"){return faRestore;}
  if (UcStr=="RESTORE"){return faRestore;}
  return faUndef;
}

/////////////////////////////////////////////////
// Files
const TStr TFile::TxtFExt=".Txt";
const TStr TFile::HtmlFExt=".Html";
const TStr TFile::HtmFExt=".Htm";
const TStr TFile::GifFExt=".Gif";
const TStr TFile::JarFExt=".Jar";

bool TFile::Exists(const TStr& FNm){
  bool DoExists;
  TFIn FIn(FNm, DoExists);
  return DoExists;
}

void TFile::Del(const TStr& FNm, const bool& ThrowExceptP){
  if (ThrowExceptP){
    EAssertR(
     remove(FNm.CStr())==0,
     "Error removing file '"+FNm+"'.");
  } else {
    remove(FNm.CStr());
  }
}

void TFile::DelWc(const TStr& WcStr, const bool& RecurseDirP){
  // collect file-names
  TStrV FNmV;
  TFFile FFile(WcStr, RecurseDirP); TStr FNm;
  while (FFile.Next(FNm)){
    FNmV.Add(FNm);}
  // delete files
  for (int FNmN=0; FNmN<FNmV.Len(); FNmN++){
    Del(FNmV[FNmN], false);}
}

void TFile::Rename(const TStr& SrcFNm, const TStr& DstFNm){
  EAssertR(
   rename(SrcFNm.CStr(), DstFNm.CStr())==0,
   "Error renaming file '"+SrcFNm+"' to "+DstFNm+"'.");
}

TStr TFile::GetUniqueFNm(const TStr& FNm){
  // <name>.#.txt --> <name>.<num>.txt
  int Cnt=1; int ch;
  TStr NewFNm; TStr TmpFNm=FNm;
  if (FNm.SearchCh('#') == -1) {
    for (ch = FNm.Len()-1; ch >= 0; ch--) if (FNm[ch] == '.') break;
    if (ch != -1) TmpFNm.InsStr(ch, ".#");
    else TmpFNm += ".#";
  }
  forever{
    NewFNm=TmpFNm;
    NewFNm.ChangeStr("#", TStr::Fmt("%03d", Cnt)); Cnt++;
    if (!TFile::Exists(NewFNm)){break;}
  }
  return NewFNm;
}


