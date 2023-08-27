
class StartingState < BaseState 

    def run
        expect_text()

        say(_?.starting.welcome)
        
        say(_?.starting.about_bot)

        suggest_it(_?.general.continue?)
            .option(_?.general.yes_) do end 
            .exec 
        
        switch_state MainMenuState.new
    end

end