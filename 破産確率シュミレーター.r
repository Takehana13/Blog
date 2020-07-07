#----破産確率関数
risk_of_ruin <-
        function(
                accuracy,
                payoff_ratio,
                mgt = 1,
                start_capital,
                fixed_percent_risked,
                ruin_point_drawdown,
                unit_of_money){
                if (mgt == 1 ){
                        money_mgt_approach = "fixed percentage risk money mgt"
                } else {
                        money_mgt_approach = "fixed dollar risk money mgt"
                }
                
                no_records = 10001
                trade_result = 0
                equity_curve = 0
                
                account_balance = start_capital
                account_new_high = start_capital
                account_drawdown_percent = 0
                number_of_trades = 1
                number_of_losses_before_ruin = 0
                number_of_trades_since_account_high = 0
                fixed_dollar_risk = start_capital / unit_of_money
                
                i = 1
                j = 1
                x = 0
                
                repeat{
                        if (account_balance > account_new_high){
                                account_new_high = account_balance
                                number_of_losses_before_ruin = 0
                                number_of_trades_since_account_high = 0
                        }
                        
                        win_of_loss = runif(1, min = 0, max = 1)
                        
                        if(win_of_loss >= (1 - accuracy)){
                                if(money_mgt_approach == "fixed percentage risk money mgt"){
                                        trade_result[j] = ((fixed_percent_risked * account_balance) * payoff_ratio)
                                }
                                if(money_mgt_approach == "fixed dollar risk money mgt"){
                                        trade_result[j] = fixed_dollar_risk * payoff_ratio
                                }
                                if(i == 1){
                                        equity_curve[i] = start_capital
                                        i = i + 1
                                        equity_curve[i] = equity_curve[i - 1] + trade_result[j]
                                } else {
                                        equity_curve[i] = equity_curve[i - 1] + trade_result[j]
                                }
                                account_balance = account_balance + trade_result[j]
                        } else {
                                if (money_mgt_approach == "fixed percentage risk money mgt"){
                                        trade_result[j] = -(fixed_percent_risked * account_balance)
                                }
                                if(money_mgt_approach == "fixed dollar risk money mgt"){
                                        trade_result[j] = -fixed_dollar_risk
                                }
                                if(i == 1){
                                        equity_curve[i] = start_capital
                                        i = i + 1
                                        equity_curve[i] = equity_curve[i - 1] + trade_result[j]
                                } else {
                                        equity_curve[i] = equity_curve[i - 1] + trade_result[j]
                                }
                                
                                account_balance = account_balance + trade_result[j]
                                account_drawdown = account_new_high - account_balance
                                account_drawdown_percent = account_drawdown / account_new_high
                                number_of_losses_before_ruin = number_of_losses_before_ruin + 1
                        }
                        number_of_trades = number_of_trades + 1
                        number_of_trades_since_account_high = number_of_trades_since_account_high + 1
                        
                        x = x + 1
                        j = j + 1
                        i = i + 1
                        
                        if(account_drawdown_percent >= ruin_point_drawdown || equity_curve[i-1] > 200000000 || x >= 10000) break
                }
                probility_of_ruin = number_of_losses_before_ruin / number_of_trades_since_account_high
                
                if(equity_curve[i-1] > 200000000 || x >= 10000){
                        probility_of_ruin = 0
                }
                print(paste0("Ruin prob = ", probility_of_ruin))
                plot(equity_curve,
                     type = "l",
                     main = "Risk of Ruin",
                     xlab = "Times",
                     ylab = "Equity")
                grid()
        }

#---- 実行テスト
accuracy             = 0.5 #勝率
payoff_ratio         = 1 #ペイオフレシオ（平均利益/平均損失）
mgt                  = 1 #マネジメントルール（1なら%、それ以外はドルベース）
start_capital        = 100 #ドル単位の当初資金
fixed_percent_risked = 0.05 #1トレードあたりの許容リスク
ruin_point_drawdown  = 0.5 #口座の破産ドローダウン
unit_of_money        = 20 #資金のユニット数

risk_of_ruin(
        accuracy,
        payoff_ratio,
        mgt,
        start_capital,
        fixed_percent_risked,
        ruin_point_drawdown,
        unit_of_money
)
