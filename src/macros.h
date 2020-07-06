#define TRIM(x_) trim(adjustl(x_))
#define LEN_TRIM(x_) len_trim(adjustl(x_))
#define POP(stack_, stackn_, number_) number_=stack_(stackn_);stackn_=stackn_-1
#define PUSH(stack_, stackn_, number_) stackn_=stackn_+1;stack_(stackn_)=number_
#define FUNCPOP(funcstack_, funcstackn_, command_, number_) funcstackn_=funcstackn_+1;command_=funcstack_%commands(funcstackn_);number_=funcstack_%numbers(funcstackn_)

