// Configuration object
const experimentConfig = {
  magnitudes: [1, 2, 5, 10],
  ratios: [1, 4, 8, 12, 16],
  trialDuration: 10000 // in milliseconds on average, U[9500, 10500]
};
experimentConfig.ratios.reverse();

// Generate trials for Vigour task
const vigourTrials = [{"magnitude":1,"ratio":16,"trialDuration":9862},{"magnitude":1,"ratio":1,"trialDuration":9506},{"magnitude":2,"ratio":12,"trialDuration":9652},{"magnitude":10,"ratio":4,"trialDuration":10221},{"magnitude":5,"ratio":8,"trialDuration":10207},{"magnitude":5,"ratio":16,"trialDuration":9890},{"magnitude":1,"ratio":4,"trialDuration":9688},{"magnitude":5,"ratio":12,"trialDuration":10145},{"magnitude":2,"ratio":1,"trialDuration":10228},{"magnitude":10,"ratio":16,"trialDuration":10386},{"magnitude":1,"ratio":8,"trialDuration":9962},{"magnitude":2,"ratio":12,"trialDuration":9985},{"magnitude":1,"ratio":4,"trialDuration":9891},{"magnitude":5,"ratio":4,"trialDuration":9956},{"magnitude":2,"ratio":8,"trialDuration":9726},{"magnitude":2,"ratio":16,"trialDuration":10236},{"magnitude":1,"ratio":8,"trialDuration":10182},{"magnitude":1,"ratio":1,"trialDuration":9501},{"magnitude":5,"ratio":12,"trialDuration":9825},{"magnitude":1,"ratio":16,"trialDuration":9902},{"magnitude":10,"ratio":8,"trialDuration":10452},{"magnitude":10,"ratio":12,"trialDuration":9954},{"magnitude":5,"ratio":16,"trialDuration":10009},{"magnitude":10,"ratio":16,"trialDuration":9827},{"magnitude":2,"ratio":16,"trialDuration":10293},{"magnitude":10,"ratio":8,"trialDuration":10376},{"magnitude":10,"ratio":12,"trialDuration":10261},{"magnitude":2,"ratio":4,"trialDuration":10490},{"magnitude":1,"ratio":12,"trialDuration":9870},{"magnitude":2,"ratio":8,"trialDuration":9535},{"magnitude":1,"ratio":12,"trialDuration":9888},{"magnitude":10,"ratio":4,"trialDuration":9679},{"magnitude":2,"ratio":1,"trialDuration":10199},{"magnitude":2,"ratio":4,"trialDuration":10044},{"magnitude":5,"ratio":4,"trialDuration":10465},{"magnitude":5,"ratio":8,"trialDuration":9666}] ;


// Global variables
window.totalReward = 0;
window.totalPresses = 0;
window.sampledVigourReward = 0;

// Functions for animation
function animatePiggy(keyframes, options) {
  const piggyBank = document.getElementById('piggy-container');
  if (piggyBank) {
    let currentTransform = getComputedStyle(piggyBank).transform;
    currentTransform = currentTransform === 'none' ? '' : currentTransform;
    const animationKeyframes = keyframes.map(frame => ({
      transform: `${currentTransform} ${frame}`
    }));
    piggyBank.animate(animationKeyframes, options);
  }
}

// Shake piggy bank animation
function shakePiggy() {
  animatePiggy([
    'translateX(-1%)',
    'translateX(1%)',
    'translateX(0)'
  ], { duration: 100, easing: 'linear' });
}

// Wiggle piggy bank animation
function wigglePiggy() {
  animatePiggy([
    `translate(0.5%, 0.5%) rotate(0deg)`,
    `translate(-0.5%, -1%) rotate(-2deg)`,
    `translate(-1.5%, 0%) rotate(2deg)`,
    `translate(1.5%, 1%) rotate(0deg)`,
    `translate(0.5%, -0.5%) rotate(2deg)`,
    `translate(-0.5%, 1%) rotate(-2deg)`,
    `translate(-1.5%, 0.5%) rotate(0deg)`,
    `translate(1.5%, 0.5%) rotate(-2deg)`,
    `translate(-0.5%, -0.5%) rotate(2deg)`,
    `translate(0.5%, 1%) rotate(0deg)` ,
    `translate(0.5%, -1%) rotate(-2deg)` 
  ], { duration: 100, easing: 'linear' });
}

// Drop coin animation
function dropCoin(magnitude, persist = false) {
  const containerType = persist ? 'persist-coin-container' : 'coin-container';
  const coinContainer = document.getElementById(containerType);
  const coin = createCoin(magnitude);
  coinContainer.appendChild(coin);

  const animationOptions = getCoinAnimationOptions(magnitude);
  coin.animate(animationOptions.keyframes, animationOptions.config)
    .onfinish = () => coin.remove();
}

function createCoin(magnitude) {
  const coin = document.createElement('img');
  coin.className = 'vigour_coin';
  coin.src = magnitude === 0 ? 'imgs/ooc_2p.png' : `imgs/${magnitude}p-num.png`;
  coin.alt = `Coin of value ${magnitude}`;
  return coin;
}

function getCoinAnimationOptions(magnitude) {
  const duration = magnitude === 0 ? 2500 : 1000;
  const topStart = '-15%';
  const opacityStart = 0.8;
  
  return {
    keyframes: [
      { top: topStart, opacity: opacityStart, offset: 0 },
      { top: '70%', opacity: 1, offset: 0.1 },
      { top: '70%', opacity: 1, offset: 0.9 },
      { top: '70%', opacity: 0, offset: 1 }
    ],
    config: {
      duration: duration,
      easing: 'ease-in-out'
    }
  };
}

// Set up an observer to handle dynamic resizing
function observeResizing(elementId, callback) {
  const resizeObserver = new ResizeObserver(callback);
  const element = document.getElementById(elementId);
  if (element) {
    resizeObserver.observe(element);
  }
}

// Update persistent coin container position based on coin container
function updatePersistentCoinContainer() {
  const coinContainer = document.getElementById('coin-container');
  const persistCoinContainer = document.getElementById('persist-coin-container');
  
  if (coinContainer && persistCoinContainer) {
    const rect = coinContainer.getBoundingClientRect();
    // console.log(rect);
    persistCoinContainer.style.top = `${rect.top}px`;
    persistCoinContainer.style.left = `${rect.left}px`;
    persistCoinContainer.style.width = `${rect.width}px`;
    persistCoinContainer.style.height = `${rect.height}px`;
  }
}

function updatePiggyTails(magnitude, ratio) {
  const piggyContainer = document.getElementById('piggy-container');
  const piggyBank = document.getElementById('piggy-bank');

  const magnitude_index = experimentConfig.magnitudes.indexOf(magnitude);
  const ratio_index = experimentConfig.ratios.indexOf(ratio);
  // Calculate saturation based on ratio
  const ratio_factor = ratio_index / (experimentConfig.ratios.length - 1);

  // Remove existing tails
  document.querySelectorAll('.piggy-tail').forEach(tail => tail.remove());

  // Wait for the piggy bank image to load
  piggyBank.onload = () => {
      const piggyBankWidth = piggyBank.offsetWidth;
      const tailWidth = piggyBankWidth * 0.1; // Adjust this factor as needed
      const spacing = tailWidth * 0; // Adjust spacing between tails
      for (let i = 0; i < magnitude_index + 1; i++) {
          const tail = document.createElement('img');
          tail.src = 'imgs/piggy-tail2.png';
          tail.alt = 'Piggy Tail';
          tail.className = 'piggy-tail';
          
          // Position each tail
          tail.style.left = `calc(50% + ${piggyBankWidth / 2 + (tailWidth + spacing) * i}px - ${tailWidth / 20}px)`;
          tail.style.width = `${tailWidth}px`;
          tail.style.filter = `saturate(${50 + 250 * ratio_factor}%)`;

          piggyContainer.appendChild(tail);
      }
  };

  // Trigger onload if the image is already cached
  if (piggyBank.complete) {
      piggyBank.onload();
  }
}

// Trial stimulus function
function generateTrialStimulus(magnitude, ratio) {
  const ratio_index = experimentConfig.ratios.indexOf(ratio);
  // Calculate saturation based on ratio
  const ratio_factor = ratio_index / (experimentConfig.ratios.length - 1);
  const piggy_style = `filter: saturate(${50 + 250 * ratio_factor}%);`;
  return `
    <div class="experiment-wrapper">
      <!-- Middle Row (Piggy Bank & Coins) -->
      <div id="experiment-container">
        <div id="coin-container"></div>
        <div id="piggy-container">
          <!-- Piggy Bank Image -->
          <img id="piggy-bank" src="imgs/piggy-bank.png" alt="Piggy Bank" style="${piggy_style}">
        </div>
      </div>
    </div>
  `;
}

// Box shaking trial
let vigourTrialCounter = 0;
const piggyBankTrial = {
  type: jsPsychHtmlKeyboardResponse,
  stimulus: function () {
    return generateTrialStimulus(jsPsych.evaluateTimelineVariable('magnitude'), jsPsych.evaluateTimelineVariable('ratio'));
  },
  choices: 'NO_KEYS',
  // response_ends_trial: false,
  trial_duration: function() {
    return jsPsych.evaluateTimelineVariable('trialDuration')
  },
  save_timeline_variables: ["magnitude", "ratio"],
  data: {
    trial_duration: () => { return jsPsych.evaluateTimelineVariable('trialDuration') },
    response_time: () => { return window.responseTime },
    trial_presses: () => { return window.trialPresses },
    trial_reward: () => { return window.trialReward },
    // Record global data
    total_presses: () => { return window.totalPresses },
    total_reward: () => { return window.totalReward }
  },
  simulation_options: {
    data: {
      trial_presses: () => { window.trialPresses = jsPsych.randomization.randomInt(8, 20) },
      trial_reward: () => { window.trialReward = Math.floor(window.trialPresses / jsPsych.evaluateTimelineVariable('ratio')) * jsPsych.evaluateTimelineVariable('magnitude') },
      response_time: () => {
        do {
          window.responseTime = [];
          for (let i = 0; i < window.trialPresses; i++) {
            window.responseTime.push(Math.floor(jsPsych.randomization.sampleExGaussian(150, 15, 0.01, true)));
          }
        } while (window.responseTime.reduce((acc, curr) => acc + curr, 0) > experimentConfig.trialDuration);
      },
      total_presses: () => { window.totalPresses += window.trialPresses },
      total_reward: () => { window.totalReward += window.trialReward }
    }
  },
  on_start: function (trial) {
    if (window.prolificPID.includes("simulate")) {
      trial.trial_duration = 1000 / 60;
    }
    // Create a shared state object
    window.trialPresses = 0;
    window.trialReward = 0;
    window.responseTime = [];

    let lastPressTime = 0;
    let pressCount = 0;

    const ratio = jsPsych.evaluateTimelineVariable('ratio');
    const magnitude = jsPsych.evaluateTimelineVariable('magnitude');

    const keyboardListener = jsPsych.pluginAPI.getKeyboardResponse({
      callback_function: function (info) {
        window.responseTime.push(info.rt - lastPressTime);
        lastPressTime = info.rt;
        // wigglePiggy();
        shakePiggy();
        pressCount++;
        window.trialPresses++;
        window.totalPresses++;

        if (pressCount === ratio) {
          window.trialReward += magnitude;
          window.totalReward += magnitude;
          pressCount = 0;
          dropCoin(magnitude, true);
        }
      },
      valid_responses: [' '],
      rt_method: 'performance',
      persist: true,
      allow_held_key: false,
      minimum_valid_rt: 0
    });
  },
  on_load: function () {
    updatePiggyTails(jsPsych.evaluateTimelineVariable('magnitude'), jsPsych.evaluateTimelineVariable('ratio'));
    updatePersistentCoinContainer(); // Update the persistent coin container
    observeResizing('coin-container', updatePersistentCoinContainer);
  },
  on_finish: function (data) {
    // Clean up listener
    jsPsych.pluginAPI.cancelAllKeyboardResponses();
    vigourTrialCounter += 1;
    data.trial_number = vigourTrialCounter;
  }
};

// Debriefing
const vigour_bonus = {
  type: jsPsychHtmlButtonResponse,
  stimulus: "Congratulations! You've finished this game!",
  choices: ['Finish'],
  on_start: function (trial) {
    const selected_trial = getSelectedTrial();
    trial.stimulus = `
            <p>Finally, it is time to reveal your bonus payment for the piggy-bank game.</p>
            <p>The computer picked round number ${selected_trial.trial_number}, which means you will earn ${(window.sampledVigourReward / 100).toLocaleString('en-GB', { style: 'currency', currency: 'GBP' })} for the game.</p>
        `;
  },
  on_finish: (data) => {
    data.vigour_bonus = window.sampledVigourReward / 100
  },
  simulation_options: {
    simulate: false
  }
};

// Create main experiment timeline
const experimentTimeline = [];
vigourTrials.forEach(trial => {
  experimentTimeline.push({
    timeline: [piggyBankTrial],
    timeline_variables: [trial]
  });
});

// Get trial reward data
function getSelectedTrial() {
  const raw_data = jsPsych.data.get().filterCustom((trial) => trial.trial_reward !== undefined);
  const trial_rewards = raw_data.select('trial_reward').values;
  // Select a random trial to be the bonus round with weights based on the rewards
  const selected_trial = jsPsych.randomization.sampleWithReplacement(raw_data.values(), 1, trial_rewards.map(reward => (reward) ** 2));
  // Side effect: Save the reward for the bonus round
  window.sampledVigourReward = selected_trial[0].trial_reward;
  // Return the trial index for referencing and the trial number for display
  return { trial_index: selected_trial[0].trial_index, trial_number: selected_trial[0].trial_number };
}
