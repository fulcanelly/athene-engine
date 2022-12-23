
class StartingState < BaseState 

    def run
        expect_text()

        say("Welcome to athene engine bot")
        
        say("
            This bot intended to help channel' owners by simplifying common tasks 

            Namely: 
            * Find mutual adverts partners easier
            * Guarantee post will be keept agreed time 
            * Automatically post ads based on settings 
        ".multitrim)


        suggest_it("Continue ?")
            .option("Yes") do end 
            .exec 
        
        switch_state MainMenuState.new
    end

end