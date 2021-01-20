import os
from msedge.selenium_tools import Edge, EdgeOptions
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions
from numpy import arange
import time

import random

# Soleus_10.0.EXP.EMG.txt
def get_data(drive, muscle, intensity, signal_type, folder=''):
    # GetData
    try:
        get_data_set = drive.find_element_by_tag_name('pre')
        dataset = get_data_set.text + '\n'
        print(f'Getting Data')
    except:
        dataset = '\n'
        print(f'Empty Data')
    text_file = open(f"{folder}{muscle}_{intensity:05.2f}.{signal_type}.txt", "w")
    print(f'File Saved: {text_file.write(dataset)} Bytes')
    text_file.close()


def get_data_page(drive=None, option='', radio='', muscle='', intensity=0.0, signal_type='', folder=''):
    remoto_window = drive.window_handles[0]
    print(f'Remoto Window: {remoto_window}')
    drive.switch_to.window(remoto_window)
    element_option = drive.find_element_by_xpath(option)
    element_option.click()
    time.sleep(2.0)
    element_radio = drive.find_element_by_xpath(radio)
    element_radio.click()
    time.sleep(2.0)
    element_export = drive.find_element_by_xpath('//*[@id="sprytrigger3"]')
    element_export.click()
    time.sleep(2.0)
    # Pega Janela original
    print(drive.window_handles)
    new_window = drive.window_handles[1]
    print(f'Data Window: {new_window}')
    drive.switch_to.window(new_window)
    get_data(drive, muscle, intensity, signal_type, folder)
    drive.close()
    drive.switch_to.window(remoto_window)


def set_run_page(drive):
    # Run
    run_page = drive.find_element_by_xpath('//*[@id="MenuBar"]/li[5]/a')
    run_page.click()
    # '/html/body/div[2]/div[2]/form/table/tbody/tr/td/table/tbody/tr[4]/td[2]/input[2]'
    try:
        run_simulation = drive.find_element_by_xpath(
            '/html/body/div[2]/div[2]/form/table/tbody/tr[3]/td/table/tbody/tr/td/table/tbody/tr[2]/td[2]/input[2]')
        run_simulation.click()
    except:
        print("1 - Fail to find start button")
        try:
            run_simulation = drive.find_element_by_xpath(
                '/html/body/div[2]/div[2]/form/table/tbody/tr/td/table/tbody/tr[4]/td[2]/input[2]')
            run_simulation.click()
            alert = WebDriverWait(drive, 5).until(expected_conditions.alert_is_present())
            alert.accept()
        except:
            print("2 - Fail to find start button")
    # Aguarda até start aparecer
    value = ''
    while value != '   Start   ':
        try:
            time.sleep(2.0)
            start_btn = drive.find_element_by_xpath(
                '/html/body/div[2]/div[2]/form/table/tbody/tr/td/table/tbody/tr[4]/td[2]/input[2]')
            value = start_btn.get_attribute('value')
            print(value)
        except:
            print('Trying again...')
            run_page = drive.find_element_by_xpath('//*[@id="MenuBar"]/li[5]/a')
            run_page.click()


def set_demo_page(drive):
    demonstrations_menu_item = drive.find_element_by_xpath('//*[@id="MenuBar"]/li[2]/a')
    demonstrations_menu_item.click()
    time.sleep(1.0)
    demo_select = drive.find_element_by_xpath('//*[@id="sprytrigger5"]/select/option[5]')
    demo_select.click()
    time.sleep(1.0)
    load_select = drive.find_element_by_xpath('//*[@id="sprytrigger4"]')
    load_select.click()
    # Alerta
    alert = WebDriverWait(drive, 5).until(expected_conditions.alert_is_present())
    alert.accept()
    time.sleep(2.0)


def set_neural_elements(drive):
    neural_config = drive.find_element_by_xpath('//*[@id="MenuBar"]/li[3]/ul/li[1]/a')
    drive.get(neural_config.get_attribute('href'))
    time.sleep(1.0)

    ptn_interneuron_checkbox = drive.find_element_by_xpath('/html/body/div[2]/div[2]/form/table/tbody/tr[3]/td['
                                                           '1]/table/tbody/tr[7]/td/input')
    ptn_interneuron_checkbox.click()
    time.sleep(1.0)
    ptn_iain_checkbox = drive.find_element_by_xpath('/html/body/div[2]/div[2]/form/table/tbody/tr[3]/td['
                                                    '1]/table/tbody/tr[8]/td/table/tbody/tr/td[2]/table/tbody/tr/td['
                                                    '1]/input')
    ptn_iain_checkbox.click()
    time.sleep(1.0)
    cpn_afferents_checkbox = drive.find_element_by_xpath('/html/body/div[2]/div[2]/form/table/tbody/tr[3]/td['
                                                         '2]/table/tbody/tr[5]/td/input')
    cpn_afferents_checkbox.click()
    time.sleep(1.0)
    cpn_ia_checkbox = drive.find_element_by_xpath('/html/body/div[2]/div[2]/form/table/tbody/tr[3]/td['
                                                  '2]/table/tbody/tr[6]/td/table/tbody/tr/td[2]/table/tbody/tr/td['
                                                  '1]/input')
    cpn_ia_checkbox.click()
    time.sleep(1.0)
    apply_config = drive.find_element_by_xpath('/html/body/div[2]/div[2]/form/table/tbody/tr[4]/td/input')
    apply_config.click()


def set_motoneuron_config(drive, s=None, fr=None, ff=None):
    if ff is None:
        ff = [13.5, 13.0]
    if fr is None:
        fr = [14.0, 13.5]
    if s is None:
        s = [20.0, 14.0]
    motoneurons_config = drive.find_element_by_xpath('//*[@id="MenuBar"]/li[3]/ul/li[3]/ul/li[3]/a')
    drive.get(motoneurons_config.get_attribute('href'))
    time.sleep(1.0)
    #
    drive.execute_script(f'document.querySelector("body > div.first > div.motoneuron > form > table > tbody > '
                         f'tr:nth-child(6) > td:nth-child(2) > table > tbody > tr:nth-child(7) > td:nth-child(1) > '
                         f'input[type=text]").value = {s[0]:.2f}')
    drive.execute_script(f'document.querySelector("body > div.first > div.motoneuron > form > table > tbody > '
                         f'tr:nth-child(6) > td:nth-child(2) > table > tbody > tr:nth-child(7) > td:nth-child(2) > '
                         f'input[type=text]").value = {s[1]:.2f}')
    drive.execute_script(f'document.querySelector("body > div.first > div.motoneuron > form > table > tbody > '
                         f'tr:nth-child(6) > td:nth-child(3) > table > tbody > tr:nth-child(7) > td:nth-child(1) > '
                         f'input[type=text]").value = {fr[0]:.2f}')
    drive.execute_script(f'document.querySelector("body > div.first > div.motoneuron > form > table > tbody > '
                         f'tr:nth-child(6) > td:nth-child(3) > table > tbody > tr:nth-child(7) > td:nth-child(2) > '
                         f'input[type=text]").value = {fr[1]:.2f}')
    drive.execute_script(f'document.querySelector("body > div.first > div.motoneuron > form > table > tbody > '
                         f'tr:nth-child(6) > td:nth-child(4) > table > tbody > tr:nth-child(7) > td:nth-child(1) > '
                         f'input[type=text]").value = {ff[0]:.2f}')
    drive.execute_script(f'document.querySelector("body > div.first > div.motoneuron > form > table > tbody > '
                         f'tr:nth-child(6) > td:nth-child(4) > table > tbody > tr:nth-child(7) > td:nth-child(2) > '
                         f'input[type=text]").value = {ff[1]:.2f}')
    time.sleep(2.0)
    apply_config = drive.find_element_by_xpath('/html/body/div[2]/div[2]/form/table/tbody/tr[11]/td/span/input')
    apply_config.click()


def set_sensory_fibers_config(drive, ia_axon_thr=None):
    # Sensory Fibers
    if ia_axon_thr is None:
        ia_axon_thr = [18.0, 9.0]
    fibers_config = reMoto.find_element_by_xpath('//*[@id="MenuBar"]/li[3]/ul/li[3]/ul/li[6]/a')
    drive.get(fibers_config.get_attribute('href'))
    time.sleep(1.0)
    drive.execute_script(
        f'document.querySelector("body > div.first > div.sensory > form > table > tbody > tr:nth-child('
        f'4) > td:nth-child(2) > table > tbody > tr:nth-child(3) > td:nth-child(1) > input'
        f'[type=text]").value = {ia_axon_thr[0]:.2f}')
    drive.execute_script(
        f'document.querySelector("body > div.first > div.sensory > form > table > tbody > tr:nth-child('
        f'4) > td:nth-child(2) > table > tbody > tr:nth-child(3) > td:nth-child(2) > input'
        f'[type=text]").value = {ia_axon_thr[1]:.2f}')
    time.sleep(2.0)
    apply_config = drive.find_element_by_xpath('/html/body/div[2]/div[2]/form/table/tbody/tr[5]/td/input')
    apply_config.click()


def set_stimulation_page(drive,
                         estimulo_ptn=10.2,
                         intervalo_ptn=None,
                         estimulo_cpn=15.0,
                         intervalo_cpn=None,
                         cpn=False):
    if intervalo_cpn is None:
        intervalo_cpn = [0.0, 1.0]
    if intervalo_ptn is None:
        intervalo_ptn = [0.0, 1.0]
    nerve_stimulation = drive.find_element_by_xpath('//*[@id="MenuBar"]/li[4]/ul/li[1]/a')
    drive.get(nerve_stimulation.get_attribute('href'))
    time.sleep(1.0)

    # Ajustando tempo
    if cpn:
        cpn_radio = drive.find_element_by_xpath('/html/body/div[2]/div[2]/form/table/tbody/tr[2]/td/table/tbody/tr['
                                                '2]/td[1]/input')

        if cpn_radio.is_selected() is False:
            cpn_radio.click()
            time.sleep(0.5)
        # CPN
        # Ajustando tempo
        drive.execute_script(
            f'document.querySelector("body > div.first > div.stimulation1 > form > table > tbody > '
            f'tr:nth-child(2) > td > table > tbody > tr:nth-child(3) > td > table > tbody > tr > '
            f'td:nth-child(3) > input[type=text]").value = {intervalo_cpn[0]:.2f}')
        # Ajustando tempo
        drive.execute_script(
            f'document.querySelector("body > div.first > div.stimulation1 > form > table > tbody > '
            f'tr:nth-child(2) > td > table > tbody > tr:nth-child(3) > td > table > tbody > tr > '
            f'td:nth-child(4) > input[type=text]").value = {intervalo_cpn[1]:.2f}')
        # Ajustando Amplitude
        drive.execute_script(f'document.querySelector("body > div.first > div.stimulation1 > form > table > tbody '
                             f'> tr:nth-child(2) > td > table > tbody > tr:nth-child(3) > td > table > tbody > tr '
                             f'> td:nth-child(5) > input[type=text]").value = {estimulo_cpn:.2f}')

        # PTN
        # Ajustando tempo
        drive.execute_script(f'document.querySelector("body > div.first > div.stimulation1 > form > table > tbody > '
                             f'tr:nth-child(2) > td > table > tbody > tr:nth-child(15) > td > table > tbody > tr > '
                             f'td:nth-child(3) > input[type=text]").value = {intervalo_ptn[0]:.2f}')
        # Ajustando tempo
        drive.execute_script(f'document.querySelector("body > div.first > div.stimulation1 > form > table > tbody > '
                             f'tr:nth-child(2) > td > table > tbody > tr:nth-child(15) > td > table > tbody > tr > '
                             f'td:nth-child(4) > input[type=text]").value = {intervalo_ptn[1]:.2f}')

        # Ajustando amplitude
        drive.execute_script(
            f'document.querySelector("body > div.first > div.stimulation1 > form > table > tbody > tr:nth-child(2) > '
            f'td > table > tbody > tr:nth-child(15) > td > table > tbody > tr > td:nth-child(5) > input['
            f'type=text]").value = {estimulo_ptn:.2f}')
    else:
        # PTN
        # Ajustando tempo
        drive.execute_script(f'document.querySelector("body > div.first > div.stimulation1 > form > table > tbody > '
                             f'tr:nth-child(2) > td > table > tbody > tr:nth-child(14) > td > table > tbody > tr > '
                             f'td:nth-child(3) > input[type=text]").value = {intervalo_ptn[0]:.2f}')
        # Ajustando tempo
        drive.execute_script(f'document.querySelector("body > div.first > div.stimulation1 > form > table > tbody > '
                             f'tr:nth-child(2) > td > table > tbody > tr:nth-child(14) > td > table > tbody > tr > '
                             f'td:nth-child(4) > input[type=text]").value = {intervalo_ptn[1]:.2f}')

        # Ajustando amplitude
        drive.execute_script(
            f'document.querySelector("body > div.first > div.stimulation1 > form > table > tbody > tr:nth-child(2) > '
            f'td > table > tbody > tr:nth-child(14) > td > table > tbody > tr > td:nth-child(5) > input['
            f'type=text]").value = {estimulo_ptn:.2f}')

    time.sleep(0.5)
    apply_stimulation = drive.find_element_by_xpath('/html/body/div[2]/div[2]/form/div/input')
    apply_stimulation.click()


def parameterization_h_reflex(drive, s=None, fr=None, ff=None, ia_axon_thr=None):
    if ia_axon_thr is None:
        ia_axon_thr = [9.0, 18.0]
    if ff is None:
        ff = [13.5, 13.0]
    if fr is None:
        fr = [14.0, 13.5]
    if s is None:
        s = [20.0, 14.0]

    # Motoneurons
    set_motoneuron_config(drive, s=s, fr=fr, ff=ff)
    time.sleep(2.0)
    # Sensory Fibers
    set_sensory_fibers_config(drive, ia_axon_thr=ia_axon_thr)
    time.sleep(2.0)


def parameterization_h_reflex_cond(drive, control=True, s=None, fr=None, ff=None, ia_axon_thr=None):
    if ia_axon_thr is None:
        ia_axon_thr = [9.0, 18.0]
    if ff is None:
        ff = [13.5, 13.0]
    if fr is None:
        fr = [14.0, 13.5]
    if s is None:
        s = [20.0, 14.0]

    if not control:
        # Neural Elements
        set_neural_elements(drive)
        time.sleep(2.0)

    parameterization_h_reflex(drive, s, fr, ff, ia_axon_thr)


########################################################################################################################
options = EdgeOptions()
options.use_chromium = True
options.add_argument("-inprivate")
options.binary_location = r"C:\Program Files (x86)\Microsoft\Edge\Application\msedge.exe"
reMoto = Edge(executable_path=r'D:\Users\Leard\edgedriver_win64\msedgedriver.exe', options=options)
reMoto.get('http://remoto.leb.usp.br/remoto/main.do')
time.sleep(2.0)
########################################################################################################################
# Menu Demonstrations
set_demo_page(reMoto)
time.sleep(2.0)
########################################################################################################################
# Parametrização
########################################################################################################################
parameterization_h_reflex(reMoto, s=[20.0, 14.0], fr=[14.0, 13.5], ff=[13.5, 13.0], ia_axon_thr=[9.0, 18.0])

########################################################################################################################
# Simulação
########################################################################################################################

experimentos_folder = ["./exp"+str(i)+"/" for i in range(12, 21)]

for experimento in experimentos_folder:
    print(f'Experiment: {experimento}')
    if not os.path.exists(experimento):
        os.makedirs(experimento)

    estimulo_ptn_values_ = arange(10.0, 24.00, 0.2)

    for estimulo_ptn in estimulo_ptn_values_:
        print(f'Simulation for {estimulo_ptn:.2f} mA')
        set_stimulation_page(drive=reMoto,
                             estimulo_ptn=estimulo_ptn,
                             intervalo_ptn=[0, 1])

        set_run_page(reMoto)
        time.sleep(1)

        results_page = reMoto.find_element_by_xpath('//*[@id="MenuBar"]/li[6]/a')
        results_page.click()
        time.sleep(1)

        # EMG
        valid = False
        while not valid:
            try:
                get_data_page(drive=reMoto,
                              option='/html/body/div[2]/div[2]/form/table/tbody/tr[3]/td[2]/table/tbody/tr['
                                     '4]/td/select/option[1]',
                              radio='/html/body/div[2]/div[2]/form/table/tbody/tr[3]/td[1]/table/tbody/tr[4]/td[1]',
                              muscle="Soleus",
                              intensity=estimulo_ptn,
                              signal_type="EXP.EMG",
                              folder=experimento)
                valid = True
            except ValueError:
                print("Waiting EXP: " + ValueError)
                time.sleep(5)
                reMoto.refresh()

        # IA
        valid = False
        while not valid:
            try:
                get_data_page(drive=reMoto,
                              option='/html/body/div[2]/div[2]/form/table/tbody/tr[3]/td[2]/table/tbody/tr['
                                     '1]/td/select/option[3]',
                              radio='/html/body/div[2]/div[2]/form/table/tbody/tr[3]/td[1]/table/tbody/tr[1]/td[1]',
                              muscle="Soleus",
                              intensity=estimulo_ptn,
                              signal_type="IA.SPIKES",
                              folder=experimento)
                valid = True
            except ValueError:
                print("Waiting IA: " + ValueError)
                time.sleep(5)
                reMoto.refresh()

        # MN
        valid = False
        while not valid:
            try:
                get_data_page(drive=reMoto,
                              option='/html/body/div[2]/div[2]/form/table/tbody/tr[3]/td[2]/table/tbody/tr['
                                     '1]/td/select/option[2]',
                              radio='/html/body/div[2]/div[2]/form/table/tbody/tr[3]/td[1]/table/tbody/tr[1]/td[1]',
                              muscle="Soleus",
                              intensity=estimulo_ptn,
                              signal_type="MN.SPIKES",
                              folder=experimento)
                valid = True
            except ValueError:
                print("Waiting MN: " + ValueError)
                time.sleep(5)
                reMoto.refresh()

########################################################################################################################
reMoto.close()
reMoto.quit()
