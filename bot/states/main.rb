class MainMenuState < BaseState 

    def run 
        expect_text
        switch_state MainMenuState.new 
    end

end
